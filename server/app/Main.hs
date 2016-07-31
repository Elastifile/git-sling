{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Concurrent (forkIO)
import           Control.Monad (when, unless, join, void)
import           Control.Monad.Except (MonadError (..))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import           Data.IORef
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as LE
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

import qualified Network.Mail.Mime as Mail
import Network.BSD (getHostName, getHostByName)
import qualified Network.BSD as Net

import qualified Filesystem.Path.CurrentOS as FP
import           Filesystem.Path.CurrentOS (FilePath)

import           System.IO (withFile, hSeek, SeekMode(..), IOMode(..), hFileSize)
import           System.IO.Error (tryIOError)
import           System.IO.Temp (createTempDirectory)
import           Text.Blaze.Html (toHtml, (!))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Concurrent (threadDelay)
import           Options.Applicative

import           Prelude hiding (FilePath)

data PrepushLogs = PrepushLogs { prepushLogDir :: FilePath, prepushFullLogFilePath :: FilePath }
    deriving (Show)

getFullHostName :: IO Net.HostName
getFullHostName = Net.hostName <$> (getHostName >>= getHostByName)

pollingInterval :: Int
pollingInterval = 1000000 * 10

encodeFP :: FilePath -> T.Text
encodeFP = T.pack . BS8.unpack . FP.encode

runPrepush :: PrepushLogs -> [String] -> Ref -> Ref -> EShell ()
runPrepush (PrepushLogs logDir logFile) cmd baseR headR = do
    let args = T.intercalate " " $ map T.pack cmd ++ [Git.refName baseR, Git.refName headR]
        env_str = "SLING_LOG_DIR=" <> encodeFP logDir
        redirect :: Int -> Text
        redirect fd = "exec " <> T.pack (show fd) <> ">> " <> encodeFP logFile <> ";"
    liftIO $ putStrLn . T.unpack $ "Executing bash with: " <> args <> " output goes to: " <> encodeFP logFile
    _output <- eprocsL "bash" ["-o", "pipefail", "-c", redirect 1 <> redirect 2 <> env_str <> " " <> args]
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

addAttachment :: FilePath -> Text -> IO (Maybe Mail.Part)
addAttachment fn attachedFN = do
    res <- liftIO $ tryIOError $ withFile (T.unpack $ encodeFP fn) ReadMode $
        \handle -> do
            size <- hFileSize handle
            -- SeekFromEnd seems broken
            hSeek handle AbsoluteSeek (max 0 (size - (1024*1024)))
            BS8.hGetContents handle
    case res of
        Left err -> do
            putStrLn $ "Failed reading from file: " <> show fn <> ", error: " <> show err
            return Nothing
        Right contents
            | BS8.length contents == 0 -> do
                  putStrLn $ "Empty data in file: " <> show fn
                  return Nothing
            | otherwise ->
                  return $ Just
                  $ Mail.Part "text/html; charset=utf-8" Mail.QuotedPrintableText (Just attachedFN) []
                  (LE.encodeUtf8 ("<html><body><pre>" <> renderHtml (toHtml $ TE.decodeUtf8 contents) <> "</pre></body></html>"))

isDryRun :: Options -> Proposal -> Bool
isDryRun options proposal = optForceDryRun options || proposalDryRun proposal

sendProposalEmail :: Options -> Proposal -> Text -> H.Html -> Maybe PrepushLogs -> EShell ()
sendProposalEmail options proposal subject body prepushLogs = do
    webHref <- liftIO $ (<> (":" <> show (optWebServerPort options))) . ("http://" <>) <$> getFullHostName
    mail1 <- liftIO $ Mail.simpleMail
        (Mail.Address Nothing $ formatEmail $ proposalEmail proposal)
        (Mail.Address Nothing $ formatEmail sourceEmail)
        ((if isDryRun options proposal then "(dry run) " else "")
         <> fromBranchName (proposalName proposal)
         <> " (" <> formatProposal proposal <> ")")
        ""
        (renderHtml $ do
                H.p . H.b $ fromString $ T.unpack subject
                body
                H.p $ H.a H.! A.href (H.preEscapedToValue webHref)  $ "Sling Server Status")
        []

    mail <- case prepushLogs of
        Nothing -> return mail1
        Just (PrepushLogs logDir fullLogOutputFile) -> do
            logPart <- liftIO $ addAttachment fullLogOutputFile "logtail.html"
            let tarName = encodeFP logDir <> ".tgz"
            _tarOut <-
                eprocsL "bash" ["-o", "pipefail", "-c",
                                "tar czvf " <> tarName <> " " <> (encodeFP logDir)]
            content <- liftIO $ LBS.readFile $ T.unpack tarName
            let tarPart = Mail.Part "application/gzip" Mail.Base64 (Just tarName) [] content
            return $ Mail.addPart (maybe id (:) logPart [tarPart]) mail1

    renderdBS <- liftIO $ Mail.renderMail' mail

    liftIO $ putStrLn . T.unpack $ "Sending email (async) to: " <> (formatEmail $ proposalEmail proposal) <> " with subject: " <> subject
    liftIO $ void $ forkIO $ runEShell $ eprocsIn (head $ optEmailClient options) ((tail $ optEmailClient options) ++ [formatEmail $ proposalEmail proposal]) $ return (L.toStrict $ LE.decodeUtf8 renderdBS)
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

slingBranchName :: Maybe Prefix -> Text -> Git.BranchName
slingBranchName Nothing suffix = mkBranchName suffix
slingBranchName (Just prefix) suffix = mkBranchName $ prefixToText prefix <> suffix

rejectProposal :: Options -> Proposal -> Text -> Maybe PrepushLogs -> (Text, ExitCode) -> EShell ()
rejectProposal options proposal reason prepushLogs (msg, err) = do
    let rejectedProposal = proposal { proposalStatus = ProposalRejected }
        rejectBranchName = formatProposal rejectedProposal
        origBranchName = formatProposal proposal
        msgBody = "REJECT " <> origBranchName <> " because: '" <> reason <> "' (" <> msg <> "), exit code = " <> T.pack (show err)
    liftIO $ putStrLn . T.unpack $ msgBody
    sendProposalEmail options proposal ("Rejecting (" <> msg <> ")") (toHtml msgBody) prepushLogs
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
    dirPath <- FP.decodeString <$> (liftIO $ createTempDirectory "/tmp" "sling.log")
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

proposalEmailHeader :: Options -> Proposal -> [Git.LogEntry] -> Maybe Text -> H.Html
proposalEmailHeader options proposal commits baseUrl = do
    H.p . H.b $ "Proposal"
    H.p . fromString $ "Proposed by: " <> (T.unpack $ formatEmail . proposalEmail $ proposal)
    H.p $ do
        H.span "Onto branch: "
        H.b (fromString . T.unpack . fromBranchName . proposalBranchOnto $ proposal)
        when (isDryRun options proposal) $ H.span " (Dry run only, branch not moved)"
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

    commitLogHtml <- proposalEmailHeader options proposal commits <$> Git.remoteUrl origin
    let title = if isDryRun options proposal
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
    logFileName <- head <$> eprocsL "mktemp" ["-p", encodeFP logDir, "prepush.XXXXXXX.txt"]
    let prepushLogs = PrepushLogs logDir (FP.fromText logFileName)
    liftIO $ modifyIORef currentState $ \state -> state { csCurrentLogFile = Just $ T.unpack logFileName }
    runPrepush prepushLogs (optCommandAndArgs options) finalBase finalHead
        `catchError` rejectProposal options proposal "Prepush command failed" (Just prepushLogs)

    liftIO $ putStrLn $ "Prepush command ran succesfully"

    case optTargetPrefix options of
        Nothing -> do
            Git.checkout (LocalBranch ontoBranchName)
            when (niceBranchName /= ontoBranchName) $
                Git.deleteLocalBranch niceBranchName & ignoreError
            if isDryRun options proposal
            then do
                sendProposalEmail options proposal "Dry-run: Prepush ran successfully" "" (Just prepushLogs)
            else do
                -- TODO ensure not dirty
                liftIO $ putStrLn . T.unpack $ "Updating: " <> fromBranchName ontoBranchName
                Git.push -- TODO -u origin master
                sendProposalEmail options proposal "Merged successfully" "" (Just prepushLogs)
        Just targetPrefix -> do
            let targetProposalName = formatProposal $ proposal { proposalPrefix = Just targetPrefix }
                targetBranchName = mkBranchName targetProposalName
            liftIO $ putStrLn $ "Creating target proposal branch: " <> T.unpack targetProposalName
            when (targetBranchName == ontoBranchName)
                $ abort $ "Can't handle branch, onto == target: " <> targetProposalName
            Git.deleteLocalBranch targetBranchName & ignoreError
            _ <- Git.createLocalBranch targetBranchName RefHead
            _ <- Git.createRemoteTrackingBranch origin targetBranchName
            Git.checkout (LocalBranch ontoBranchName)
            Git.deleteLocalBranch targetBranchName
            sendProposalEmail options proposal ("Ran successfully, moved to: " <> prefixToText targetPrefix) "" (Just prepushLogs)
            return ()

    Git.checkout (LocalBranch ontoBranchName)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin ontoBranchName)

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
    , optBranchExcludeFilterAll :: Maybe String
    , optBranchFilterDryRun :: Maybe String
    , optBranchFilterNoDryRun :: Maybe String
    , optWebServerPort :: Int
    , optEmailClient :: [Text]
    , optProposalMode :: ProposalMode
    , optForceDryRun :: Bool
    , optSourcePrefix :: Maybe Prefix
    , optTargetPrefix :: Maybe Prefix
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
      long "Process proposals from current (no polling) branches: Fetch current branches and process all existing proposals, then exit"))
    <|> (ProposalFromBranch <$>
            (optional $ option auto
                (short 'd' <>
                 metavar "T" <>
                 long "Poll proposals from branches, checking git status every T seconds")))
    <|> (ProposalFromCommandLine . parseProposalFromCmdLine <$>
         (strOption (long "proposal-branch" <>
                     short 'b' <>
                     help "A proposal branch name to handle")))

prefixOption :: Mod OptionFields String -> Parser Prefix
prefixOption args = prefixFromText . verify . T.pack <$> strOption args
    where
        verify t = if T.null t then error "Prefix can't be empty" else t

parser :: Parser Options
parser = Options
    <$> (optional $ strOption
         (long "match-branches" <>
          metavar "PATTERN" <>
          help "Regex pattern to match 'onto' branch name in any proposal."))
    <*> (optional $ strOption
         (long "exclude-branches" <>
          metavar "PATTERN" <>
          help "Regex pattern to exclude 'onto' branch name in any proposal."))
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
    <*> (switch
         (short 'n' <>
          long "force-dry-run" <>
          help ("Treat all proposals as dry run (regardless of what they say)")))
    <*> (optional $ prefixOption
         (long "source-prefix" <>
          help ("Prefix of branches to be used for proposals")))
    <*> (optional $ prefixOption
         (long "target-prefix" <>
          help ("If missing, successful branches are merged (unless dry-run) & deleted. If non-empty, prefix of branches to be used for succesful proposals (branches will not be merged).")))
    <*> (some $ argument str
         (metavar "-- COMMAND" <>
          help "Pre-push command to run on each proposed branch (exit code 0 considered success)"))


shouldConsiderProposal :: Options -> Proposal -> Bool
shouldConsiderProposal options proposal =
    (ProposalProposed == proposalStatus proposal)
    && (optSourcePrefix options == proposalPrefix proposal)
    && (fromMaybe True $ checkFilter <$> optBranchFilterAll options)
    && (fromMaybe True $ not . checkFilter <$> optBranchExcludeFilterAll options)
    && (fromMaybe True $ ((not $ proposalDryRun proposal) ||) . checkFilter <$> optBranchFilterDryRun options)
    && (fromMaybe True $ (proposalDryRun proposal ||) . checkFilter <$> optBranchFilterNoDryRun options)
    where checkFilter pat = (T.unpack . fromBranchName $ proposalBranchOnto proposal) =~ pat

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
        , maybe "" ("match filter: " <> ) (optBranchFilterAll options)
        , maybe "" ("exclude filter: " <> ) (optBranchExcludeFilterAll options)
        , maybe "" ("dry run branch filter: " <> ) (optBranchFilterDryRun options)
        , maybe "" ("non-dry-run branch filter: " <> ) (optBranchFilterNoDryRun options)
        ]
    liftIO $ putStrLn $ mconcat $ List.intersperse "\n\t"
        [ "Prefixes: "
        , "Source: " <> maybe "" (T.unpack . prefixToText) (optSourcePrefix options)
        , "Target: " <> maybe "" (T.unpack . prefixToText) (optTargetPrefix options)
        ]

    let allProposals =
            List.sortOn (proposalQueueIndex . snd)
            $ mapMaybe (\branch -> (branch,) <$> parseProposal (branchName branch)) (map (uncurry Git.RemoteBranch) remoteBranches)
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
serverLoop currentState mInterval options = do
    void $ liftIO $ forkServer (optWebServerPort options) (readIORef currentState)
    let go = do
            havePending <- serverPoll currentState options
            if havePending
                then go
                else case mInterval of
                    Nothing -> return ()
                    Just interval -> do
                        liftIO $ threadDelay $ interval*1000000
                        go
    go

main :: IO ()
main = runEShell $ do
    options <- liftIO $ parseOpts
    currentState <- liftIO $ newIORef emptyCurrentState
    case optProposalMode options of
        ProposalFromBranch interval -> serverLoop currentState interval options
        ProposalFromCommandLine proposal -> handleSpecificProposal currentState options proposal
