{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Monad (forM_, when, unless, join)
import           Control.Monad.Except (MonadError (..))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Sling.Git                     (Branch (..), Ref (..),
                                                Remote (..), branchName,
                                                fromBranchName, mkBranchName)
import qualified Sling.Git as Git
import           Sling.Lib                     (EShell, Email (..), abort,
                                                eprocsIn, eprocsL, formatEmail,
                                                ignoreError, runEShell, fromHash)
import           Sling.Proposal
import           Sling.Web (forkServer, CurrentState(..), emptyCurrentState)
import           Text.Regex.Posix ((=~))
import           Turtle (ExitCode, (&))

import qualified Data.List as List

import           Network.Mail.Mime (Mail)
import qualified Network.Mail.Mime as Mail

import           System.IO.Temp (createTempDirectory)
import           Text.Blaze.Html (toHtml, (!))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Concurrent (killThread, threadDelay)
import           Options.Applicative

pollingInterval :: Int
pollingInterval = 1000000 * 10

runPrepush :: FilePath -> [String] -> Ref -> Ref -> EShell ()
runPrepush logFile cmd baseR headR = do
    let args = T.intercalate " " $ map T.pack cmd ++ [Git.refName baseR, Git.refName headR]
    liftIO $ putStrLn . T.unpack $ "Executing bash with: " <> args <> " output goes to: " <> T.pack logFile
    _output <- eprocsL "bash" ["-c", args <> " &>" <> T.pack logFile]
    -- TODO delete log if successful?
    return ()

origin :: Remote
origin = Remote "origin"

sourceEmail :: Email
sourceEmail = Email "elasti-prepush" "elastifile.com"

resetLocalOnto :: Proposal -> EShell ()
resetLocalOnto proposal = do
    let ontoBranchName = proposalBranchOnto proposal
    Git.checkout (LocalBranch ontoBranchName)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin ontoBranchName)

addAttachment :: Text -> FilePath -> Text -> Mail -> IO Mail
addAttachment ct fn attachedFN mail = do
    content <- LBS.readFile fn
    let part = Mail.Part ct Mail.QuotedPrintableText (Just attachedFN) []
               (encodeUtf8 ("<html><body><pre>" <> renderHtml (toHtml . L.toStrict $ decodeUtf8 content) <> "</pre></body></html>"))
    return $ Mail.addPart [part] mail

sendProposalEmail :: Options -> Proposal -> Text -> H.Html -> Maybe FilePath -> EShell ()
sendProposalEmail options proposal subject body logFile = do
    mail1 <- liftIO $ Mail.simpleMail
        (Mail.Address Nothing $ formatEmail $ proposalEmail proposal)
        (Mail.Address Nothing $ formatEmail sourceEmail)
        ((if proposalDryRun proposal then "(dry run) " else "")
         <> fromBranchName (proposalName proposal)
         <> " (" <> formatProposal proposal <> ")")
        ""
        (renderHtml $ do
                H.p . H.b $ fromString $ T.unpack subject
                body)
        []

    mail <- case logFile of
        Nothing -> return mail1
        Just f -> liftIO $ addAttachment "text/html; charset=utf-8" f "log.html" mail1

    renderdBS <- liftIO $ Mail.renderMail' mail

    liftIO $ putStrLn . T.unpack $ "Sending email to: " <> (formatEmail $ proposalEmail proposal) <> " with subject: " <> subject
    _ <- eprocsIn (head $ optEmailClient options) ((tail $ optEmailClient options) ++ [formatEmail $ proposalEmail proposal]) $ return (L.toStrict $ decodeUtf8 renderdBS)
    return ()


clearCurrentProposal :: IORef CurrentState -> IO ()
clearCurrentProposal currentState = modifyIORef currentState $ \state -> state { csCurrentProposal = Nothing, csCurrentLogFile = Nothing }

abortAttempt :: IORef CurrentState -> Options -> Proposal -> ExitCode -> EShell ()
abortAttempt currentState options proposal _err = do
    liftIO $ putStrLn "ABORTING..."
    sendProposalEmail options proposal "Aborting" "" Nothing
    Git.rebaseAbort & ignoreError
    Git.reset Git.ResetHard RefHead
    resetLocalOnto proposal
    liftIO $ clearCurrentProposal currentState
    abort "Aborted"

rejectProposal :: Options -> Proposal -> Text -> Maybe FilePath -> ExitCode -> EShell ()
rejectProposal options proposal msg logFile err = do
    let rejectedProposal = proposal { proposalStatus = ProposalRejected }
        rejectBranchName = formatProposal rejectedProposal
        origBranchName = formatProposal proposal
        msgBody = "REJECT " <> origBranchName <> " because: '" <> msg <> "', exit code = " <> T.pack (show err)
    liftIO $ putStrLn . T.unpack $ msgBody
    sendProposalEmail options proposal ("Rejecting (" <> msg <> ")") (toHtml msgBody) logFile
    Git.fetch & ignoreError
    Git.deleteBranch (RemoteBranch origin $ mkBranchName rejectBranchName) & ignoreError -- in case it exists
    Git.deleteBranch (LocalBranch $ mkBranchName rejectBranchName) & ignoreError -- in case it exists
    Git.reset Git.ResetHard RefHead
    -- We have to be on another branch before deleting stuff, so arbitrarily picking onto
    Git.checkout (LocalBranch $ proposalBranchOnto proposal)
    _ <- Git.createLocalBranch (mkBranchName rejectBranchName) RefHead
    _ <- Git.createRemoteTrackingBranch origin $ mkBranchName rejectBranchName
    Git.deleteBranch (LocalBranch . mkBranchName $ formatProposal proposal) & ignoreError
    Git.deleteBranch (RemoteBranch origin $ mkBranchName origBranchName)
    abort "Rejected"

attemptBranchOrAbort :: IORef CurrentState -> Options -> Branch -> Proposal -> EShell ()
attemptBranchOrAbort currentState options branch proposal = do
    dirPath <- liftIO $ createTempDirectory "/tmp" "sling.log"
    attemptBranch currentState options dirPath branch proposal `catchError` abortAttempt currentState options proposal

htmlFormatCommit :: Maybe Text -> Git.LogEntry -> H.Html
htmlFormatCommit urlPrefix l = do
    let hashCol =
            case (join $ Git.githubCommitUrl (Git.logEntryFullHash l) <$> urlPrefix) of
                Nothing -> fromString . T.unpack $ fromHash $ Git.logEntryShortHash l
                Just url -> H.a ! A.href (fromString $ T.unpack url) $ fromString . T.unpack $ fromHash $ Git.logEntryShortHash l
    H.td hashCol
    H.td $ fromString $ T.unpack $ Git.logEntryAuthor l
    H.td $ fromString $ T.unpack $ Git.logEntryTitle l

htmlFormatCommitLog :: [Git.LogEntry] -> Maybe Text -> H.Html
htmlFormatCommitLog commits urlPrefix = do
    H.p "Commits:"
    H.table . H.tbody $ mapM_ (H.tr . htmlFormatCommit urlPrefix) commits

proposalEmailHeader :: Proposal -> [Git.LogEntry] -> Maybe Text -> H.Html
proposalEmailHeader proposal commits baseUrl = do
    H.p . H.b $ "Proposal"
    H.p . fromString $ "Proposed by: " <> (T.unpack $ formatEmail . proposalEmail $ proposal)
    H.p $ do
        H.span "Onto branch: "
        H.b (fromString . T.unpack . fromBranchName . proposalBranchOnto $ proposal)
        when (proposalDryRun proposal) $ H.span " (Dry run only, branch not moved)"
    htmlFormatCommitLog commits baseUrl

attemptBranch :: IORef CurrentState -> Options -> FilePath -> Branch -> Proposal -> EShell ()
attemptBranch currentState options logDir branch proposal = do
    Git.fetch

    liftIO $ modifyIORef currentState $ \state -> state { csCurrentProposal = Just proposal }
    liftIO $ putStrLn . T.unpack $ "Attempting proposal: " <> formatProposal proposal
    liftIO $ putStrLn "Commits: "
    commits <- Git.log (proposalBranchBase proposal) (RefBranch branch)
    liftIO $ mapM_ print commits

    commitLogHtml <- proposalEmailHeader proposal commits <$> Git.remoteUrl origin
    let title = if proposalDryRun proposal
                then "Running dry run"
                else "Attempting to merge"
    sendProposalEmail options proposal title commitLogHtml Nothing

    -- cleanup leftover state from previous runs
    Git.rebaseAbort & ignoreError
    Git.reset Git.ResetHard RefHead
    resetLocalOnto proposal

    remoteBranches <- Git.remoteBranches

    let niceBranchName = proposalName proposal
        niceBranch = LocalBranch niceBranchName
        ontoBranchName = proposalBranchOnto proposal
        remoteOnto = RefBranch $ RemoteBranch origin ontoBranchName
        verifyRemoteBranch rb =
            unless (elem rb remoteBranches)
            $ abort $ "No remote branch: " <> T.pack (show rb)

    verifyRemoteBranch (origin, ontoBranchName)

    -- note: 'nice' branch and 'onto' branch may be the same
    -- branch. (e.g. proposal called 'master' with onto=master)

    -- sync local onto with remote
    Git.checkout (LocalBranch ontoBranchName)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin ontoBranchName)
    finalBase <- Git.currentRef

    -- create local work branch, reset to proposed
    Git.deleteBranch niceBranch & ignoreError
    (Git.createLocalBranch niceBranchName RefHead >> pure ()) & ignoreError
    Git.checkout niceBranch
    Git.reset Git.ResetHard (RefBranch branch)

    -- rebase work onto target
    Git.rebase (proposalBranchBase proposal) remoteOnto
        `catchError` rejectProposal options proposal "Rebase failed" Nothing

    liftIO $ putStrLn "Commits (after rebase): "
    commitsAfter <- Git.log (proposalBranchBase proposal) (RefBranch branch)
    liftIO $ mapM_ print commitsAfter

    -- go back to 'onto', decide whether to create a merge commit on
    -- top (if we should merge ff only)
    Git.checkout (LocalBranch ontoBranchName)

    isMerge <- Git.isMergeCommit (RefBranch branch)
    let mergeFF =
            if isMerge || (length commits == 1)
            then Git.MergeFFOnly
            else Git.MergeNoFF
    Git.merge mergeFF niceBranch
    when (mergeFF == Git.MergeNoFF) $
        Git.commitAmend (proposalEmail proposal) Git.RefHead

    finalHead <- Git.currentRef

    -- Fast-forward the work branch to match the merged 'onto' we do
    -- this so that the prepush script will see itself running on a
    -- branch with the name the user gave to this proposal, and not
    -- the onto branch's name.
    Git.checkout niceBranch
    Git.merge Git.MergeFFOnly (LocalBranch ontoBranchName)

    -- DO IT!
    logFileName <- T.unpack . head <$> eprocsL "mktemp" ["-p", T.pack logDir, "prepush.XXXXXXX.txt"]
    liftIO $ modifyIORef currentState $ \state -> state { csCurrentLogFile = Just logFileName }
    runPrepush logFileName (optCommandAndArgs options) finalBase finalHead
        `catchError` rejectProposal options proposal "Prepush command failed" (Just logFileName)

    liftIO $ putStrLn . T.unpack $ "Updating: " <> fromBranchName ontoBranchName

    Git.checkout (LocalBranch ontoBranchName)
    when (niceBranchName /= ontoBranchName) $
        Git.deleteLocalBranch niceBranchName & ignoreError

    if proposalDryRun proposal
    then do
        sendProposalEmail options proposal "Dry-run: Prepush ran successfully" "" (Just logFileName)
    else do
        -- TODO ensure not dirty
        Git.push -- TODO -u origin master
        sendProposalEmail options proposal "Merged successfully" "" (Just logFileName)

    -- TODO delete logfile name

    liftIO $ putStrLn "Deleting proposal branch..."
    Git.deleteBranch branch

    liftIO $ putStrLn . T.unpack $ "Finished handling proposal " <> formatProposal proposal

usage :: String
usage = List.intercalate "\n"
    [ "Usage: sling-server COMMAND"
    , ""
    , "where COMMAND is the prepush command to run on each attempted branch."
    ]

data Options =
    Options
    { optBranchFilterAll :: Maybe String
    , optBranchFilterDryRun :: Maybe String
    , optBranchFilterNoDryRun :: Maybe String
    , optWebServerPort :: Int
    , optEmailClient :: [Text]
    , optPollingInterval :: Maybe Int
    , optCommandAndArgs :: [String]
    }

parseOpts :: IO Options
parseOpts = execParser $
    info (helper <*> parser)
    (fullDesc <> header "git-sling - merge branches with due process")

defaultEmailClient :: [Text]
defaultEmailClient = ["msmtp", "-C", "/opt/msmtp.conf"]

defaultPort :: Int
defaultPort = 8080

parser :: Parser Options
parser = Options
    <$> (optional $ strOption
         (long "match-branches" <>
          metavar "PATTERN" <>
          help "Regex pattern to match 'onto' branch name in any proposal."))
    <*> (optional $ strOption
         (long "match-dry-run-branches" <>
          metavar "PATTERN" <>
          help "Regex pattern to match 'onto' branch name in dry run proposals."))
    <*> (optional $ strOption
         (long "match-non-dry-run-branches" <>
          metavar "PATTERN" <>
          help "Regex pattern to match 'onto' branch name in non-dry run proposals."))
    <*> (option auto
          (value defaultPort <>
           short 'p' <>
           long "port" <>
           metavar "PORT" <>
           help ("Port for sling web server. Default: " <> (show defaultPort))))
    <*> (fmap (fromMaybe defaultEmailClient . fmap (T.words . T.pack)) <$>
         optional $ strOption
         (short 'e' <>
          long "email-client" <>
          metavar "COMMAND" <>
          help ("Command to use sending emails. Default: " <> (T.unpack $ T.intercalate " " defaultEmailClient))))
    <*> (optional $ option auto
         (short 'd' <>
          metavar "T" <>
          long "keep running, polling git status every T seconds"))
    <*> (some $ argument str
         (metavar "-- COMMAND" <>
          help "Pre-push command to run on each proposed branch (exit code 0 considered success)"))


shouldConsiderProposal :: Options -> Proposal -> Bool
shouldConsiderProposal options proposal =
    (fromMaybe True $ checkFilter <$> optBranchFilterAll options)
    && (fromMaybe True $ ((not isDryRun) ||) . checkFilter <$> optBranchFilterDryRun options)
    && (fromMaybe True $ (isDryRun ||) . checkFilter <$> optBranchFilterNoDryRun options)
    where checkFilter pat = (T.unpack . fromBranchName $ proposalBranchOnto proposal) =~ pat
          isDryRun = proposalDryRun proposal

serverPoll :: IORef CurrentState -> Options -> EShell ()
serverPoll currentState options = do
    Git.fetch
    remoteBranches <- Git.remoteBranches

    liftIO $ putStrLn $ mconcat $ List.intersperse "\n\t"
        [ "Filters: "
        , maybe "" ("branch filter: " <> ) (optBranchFilterAll options)
        , maybe "" ("dry run branch filter: " <> ) (optBranchFilterDryRun options)
        , maybe "" ("non-dry-run branch filter: " <> ) (optBranchFilterNoDryRun options)
        ]
    let proposedBranches =
            List.sort
            $ filter (\b -> proposedPrefix `T.isPrefixOf` (fromBranchName $ snd b)) remoteBranches

        allProposals =
            mapMaybe (\branch -> (branch,) <$> parseProposal (branchName branch)) (map (uncurry Git.RemoteBranch) proposedBranches)
        proposals = filter (shouldConsiderProposal options . snd) allProposals

        showProposals ps = List.intercalate "\n\t" (map (show . branchName . fst) ps)

    liftIO $ putStrLn $ "All proposals (before filtering):\n\t" <> showProposals allProposals
    liftIO $ putStrLn $ "Going to attempt proposals:\n\t" <> showProposals proposals

    liftIO $ modifyIORef currentState $ \state -> state { csPendingProposals = map snd proposals }
    forM_ proposals (uncurry $ attemptBranchOrAbort currentState options)

    liftIO $ clearCurrentProposal currentState
    liftIO $ putStrLn $ "Done."

serverLoop :: IORef CurrentState -> Options -> EShell ()
serverLoop currentState options = do
    let go = do
            serverPoll currentState options
            case optPollingInterval options of
                Nothing -> return ()
                Just d -> do
                    liftIO $ threadDelay $ d*1000000
                    go
    go

main :: IO ()
main = runEShell $ do
    options <- liftIO $ parseOpts
    currentState <- liftIO $ newIORef emptyCurrentState
    serverThread <- liftIO $ forkServer (optWebServerPort options) (readIORef currentState)
    _ <- serverLoop currentState options
    liftIO $ killThread serverThread
