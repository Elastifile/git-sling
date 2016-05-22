{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Monad (when, unless, join)
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
import           Data.Time.Clock.POSIX (getPOSIXTime)
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
import Network.BSD (getHostName, getHostByName)
import qualified Network.BSD as Net

import           System.IO.Temp (createTempDirectory)
import           Text.Blaze.Html (toHtml, (!))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Concurrent (killThread, threadDelay)
import           Options.Applicative

getFullHostName :: IO Net.HostName
getFullHostName = Net.hostName <$> (getHostName >>= getHostByName)

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
    webHref <- liftIO $ (<> (":" <> show (optWebServerPort options))) . ("http://" <>) <$> getFullHostName
    mail1 <- liftIO $ Mail.simpleMail
        (Mail.Address Nothing $ formatEmail $ proposalEmail proposal)
        (Mail.Address Nothing $ formatEmail sourceEmail)
        ((if proposalDryRun proposal then "(dry run) " else "")
         <> fromBranchName (proposalName proposal)
         <> " (" <> formatProposal proposal <> ")")
        ""
        (renderHtml $ do
                H.p . H.b $ fromString $ T.unpack subject
                body
                H.p $ H.a H.! A.href (H.preEscapedToValue webHref)  $ "Sling Server Status")
        []

    mail <- case logFile of
        Nothing -> return mail1
        Just f -> liftIO $ addAttachment "text/html; charset=utf-8" f "log.html" mail1

    renderdBS <- liftIO $ Mail.renderMail' mail

    liftIO $ putStrLn . T.unpack $ "Sending email to: " <> (formatEmail $ proposalEmail proposal) <> " with subject: " <> subject
    _ <- eprocsIn (head $ optEmailClient options) ((tail $ optEmailClient options) ++ [formatEmail $ proposalEmail proposal]) $ return (L.toStrict $ decodeUtf8 renderdBS)
    return ()


clearCurrentProposal :: IORef CurrentState -> IO ()
clearCurrentProposal currentState = do
    modifyIORef currentState $ \state ->
        state
        { csCurrentProposal = Nothing
        , csCurrentLogFile = Nothing }

abortAttempt :: IORef CurrentState -> Options -> Proposal -> (Text, ExitCode) -> EShell ()
abortAttempt currentState options proposal (msg, _err) = do
    liftIO $ putStrLn $ "ABORTING: " ++ (show msg)
    sendProposalEmail options proposal "Aborting" (H.html $ H.text msg) Nothing
    Git.rebaseAbort & ignoreError
    Git.reset Git.ResetHard RefHead
    resetLocalOnto proposal
    liftIO $ clearCurrentProposal currentState
    abort "Aborted"

rejectProposal :: Options -> Proposal -> Text -> Maybe FilePath -> (Text, ExitCode) -> EShell ()
rejectProposal options proposal reason logFile (msg, err) = do
    let rejectedProposal = proposal { proposalStatus = ProposalRejected }
        rejectBranchName = formatProposal rejectedProposal
        origBranchName = formatProposal proposal
        msgBody = "REJECT " <> origBranchName <> " because: '" <> reason <> "' (" <> msg <> "), exit code = " <> T.pack (show err)
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

    time <- liftIO getPOSIXTime
    liftIO $ modifyIORef currentState $ \state ->
        state
        { csCurrentProposal = Just (proposal, time)
        }
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
    Git.rebase Git.Rebase { Git.rebaseBase = proposalBranchBase proposal,
                            Git.rebaseOnto = remoteOnto,
                            Git.rebasePolicy = Git.RebaseDropMerges
                          }
        `catchError` rejectProposal options proposal "Rebase failed" Nothing

    liftIO $ putStrLn "Commits (after rebase): "
    commitsAfter <- Git.log (proposalBranchBase proposal) (RefBranch branch)
    liftIO $ mapM_ print commitsAfter

    -- go back to 'onto', decide whether to create a merge commit on
    -- top (if we should merge ff only)
    Git.checkout (LocalBranch ontoBranchName)

    isMerge <- Git.isMergeCommit (RefBranch niceBranch)
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

data ProposalMode
    = ProposalFromBranch (Maybe Int)
    | ProposalFromCommandLine Proposal

data Options =
    Options
    { optBranchFilterAll :: Maybe String
    , optBranchFilterDryRun :: Maybe String
    , optBranchFilterNoDryRun :: Maybe String
    , optWebServerPort :: Int
    , optEmailClient :: [Text]
    , optProposalMode :: ProposalMode
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

parseProposalFromCmdLine :: String -> Proposal
parseProposalFromCmdLine s =
    case parseProposal (T.pack s) of
        Nothing -> error $ "Invalid proposal format: " ++ show s
        Just p -> p

parseModeBranches :: Parser ProposalMode
parseModeBranches =
    (flag' (ProposalFromBranch Nothing)
     (short 's' <>
      long "Read a single proposal from branch"))
    <|> (ProposalFromBranch <$>
            (optional $ option auto
                (short 'd' <>
                 metavar "T" <>
                 long "Poll proposals from branches, checking git status every T seconds")))
    <|> (ProposalFromCommandLine . parseProposalFromCmdLine <$>
         (strOption (long "proposal-branch" <>
                     short 'b' <>
                     help "A proposal branch name to handle")))

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
    <*> parseModeBranches
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

handleSpecificProposal :: IORef CurrentState -> Options -> Proposal -> EShell ()
handleSpecificProposal state options proposal = do
    Git.fetch
    remoteBranches <- Git.remoteBranches
    let proposalBranchName = formatProposal proposal
        matchingBranches = filter (\b -> proposalBranchName == fromBranchName (snd b)) remoteBranches
    branch <- case matchingBranches of
        [] -> abort $ "Failed to find branch: " <> (T.pack $ show proposalBranchName)
        [b] -> return b
        _ -> abort $ "Assertion failed: multiple branches matching the same proposal: " <> (T.pack $ show proposalBranchName)
    attemptBranchOrAbort state options (uncurry Git.RemoteBranch $ branch) proposal

serverPoll :: IORef CurrentState -> Options -> EShell Bool
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
            filter (\b -> proposedPrefix `T.isPrefixOf` (fromBranchName $ snd b)) remoteBranches

        allProposals =
            List.sortOn (proposalQueueIndex . snd)
            $ mapMaybe (\branch -> (branch,) <$> parseProposal (branchName branch)) (map (uncurry Git.RemoteBranch) proposedBranches)
        proposals = filter (shouldConsiderProposal options . snd) allProposals

        showProposals ps = List.intercalate "\n\t" (map (show . branchName . fst) ps)

    liftIO $ putStrLn $ "All proposals (before filtering):\n\t" <> showProposals allProposals
    liftIO $ putStrLn $ "Going to attempt proposals:\n\t" <> showProposals proposals

    liftIO $ modifyIORef currentState $ \state -> state { csPendingProposals = map snd proposals }
    case proposals of
        [] -> do
            liftIO $ putStrLn $ "Done - have nothing to do."
            return False
        (topProposal:_) -> do
            (uncurry $ attemptBranchOrAbort currentState options) topProposal
            liftIO $ clearCurrentProposal currentState
            liftIO $ putStrLn $ "Done proposal: " ++ (show topProposal)
            return True

serverLoop :: IORef CurrentState -> Maybe Int -> Options -> EShell ()
serverLoop currentState interval options = do
    let go = do
            havePending <- serverPoll currentState options
            if havePending
                then go
                else case interval of
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
    case optProposalMode options of
        ProposalFromBranch interval -> serverLoop currentState interval options
        ProposalFromCommandLine proposal -> handleSpecificProposal currentState options proposal
    liftIO $ killThread serverThread
