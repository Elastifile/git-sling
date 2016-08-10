{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

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
import           Sling.Lib                     (EShell, Email (..), abort, eview, eproc,
                                                eprocsIn, eprocsL, formatEmail, eprint,
                                                ignoreError, runEShell, fromHash)
import           Sling.Proposal
import           Sling.Web (forkServer, CurrentState(..), emptyCurrentState)
import           Text.Regex.Posix ((=~))
import           Turtle (ExitCode, (&), echo)

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
import           Control.Concurrent (forkIO, threadDelay)
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
        bashArgs = [ "-o", "pipefail", "-c"
                   , "exec 2>&1; " <> env_str <> " " <> args
                     <> " | tee " <> encodeFP logFile]
    eprint $ "Executing bash with: '" <> mconcat bashArgs <> "' output goes to: " <> encodeFP logFile
    eprint "----------------------------------------------------------------------"
    eproc "bash" bashArgs (return "")
    eprint "----------------------------------------------------------------------"
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
            echo . T.pack $ "Failed reading from file: " <> show fn <> ", error: " <> show err
            return Nothing
        Right contents
            | BS8.length contents == 0 -> do
                  echo . T.pack $ "Empty data in file: " <> show fn
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
                                "tar czvf " <> tarName <> " " <> encodeFP logDir]
            content <- liftIO $ LBS.readFile $ T.unpack tarName
            let tarPart = Mail.Part "application/gzip" Mail.Base64 (Just tarName) [] content
            return $ Mail.addPart (maybe id (:) logPart [tarPart]) mail1

    renderdBS <- liftIO $ Mail.renderMail' mail

    eprint $ "Sending email (async) to: " <> formatEmail (proposalEmail proposal) <> " with subject: " <> subject
    liftIO $ void $ forkIO $ runEShell $ eprocsIn (head $ optEmailClient options) (tail (optEmailClient options) ++ [formatEmail $ proposalEmail proposal]) $ return (L.toStrict $ LE.decodeUtf8 renderdBS)
    return ()


clearCurrentProposal :: IORef CurrentState -> IO ()
clearCurrentProposal currentState =
    modifyIORef currentState $ \state ->
        state
        { csCurrentProposal = Nothing
        , csCurrentLogFile = Nothing }

cleanupGit :: Proposal -> EShell ()
cleanupGit proposal = do
    Git.rebaseAbort & ignoreError
    Git.reset Git.ResetHard RefHead
    resetLocalOnto proposal

abortAttempt :: IORef CurrentState -> Options -> Proposal -> (Text, ExitCode) -> EShell ()
abortAttempt currentState options proposal (msg, _err) = do
    eprint . T.pack $ "ABORTING: " ++ show msg
    sendProposalEmail options proposal "Aborting" (H.html $ H.text msg) Nothing
    cleanupGit proposal
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
    eprint $ msgBody
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
    dirPath <- FP.decodeString <$> liftIO (createTempDirectory "/tmp" "sling.log")
    attemptBranch currentState options dirPath branch proposal `catchError` abortAttempt currentState options proposal

htmlFormatCommit :: Maybe Text -> Git.LogEntry -> H.Html
htmlFormatCommit urlPrefix l = do
    let hashCol =
            case join $ Git.githubCommitUrl (Git.logEntryFullHash l) <$> urlPrefix of
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
    H.p . fromString $ "Proposed by: " <> T.unpack (formatEmail . proposalEmail $ proposal)
    H.p $ do
        H.span "Onto branch: "
        H.b (fromString . T.unpack . fromBranchName . proposalBranchOnto $ proposal)
        when (isDryRun options proposal) $ H.span " (Dry run only, branch not moved)"
    htmlFormatCommitLog commits baseUrl

setStateProposal :: (Show a, Foldable t) => IORef CurrentState -> Proposal -> t a -> IO ()
setStateProposal currentState proposal commits = do
    time <- getPOSIXTime
    modifyIORef currentState $ \state ->
        state
        { csCurrentProposal = Just (proposal, time)
        }
    echo $ "Attempting proposal: " <> formatProposal proposal
    echo "Commits: "
    mapM_ print commits

safeCreateBranch :: Git.BranchName -> EShell ()
safeCreateBranch targetBranchName = do
    Git.deleteLocalBranch targetBranchName & ignoreError
    _ <- Git.createLocalBranch targetBranchName RefHead
    _ <- Git.createRemoteTrackingBranch origin targetBranchName
    return ()

withNewBranch :: Git.BranchName -> EShell a -> EShell a
withNewBranch b act = do
    safeCreateBranch b
    let cleanup = do
            Git.deleteBranch (Git.LocalBranch b)
            Git.deleteBranch (RemoteBranch origin b)
    res <- act `catchError` (\e -> cleanup >> throwError e)
    cleanup
    return res

withLocalBranch :: Git.BranchName -> EShell () -> EShell ()
withLocalBranch name act = do
    Git.deleteBranch branch & ignoreError
    Git.localBranches >>= (liftIO . mapM_ print)
    shouldCreate <- not . elem name <$> Git.localBranches
    when shouldCreate $ void $ Git.createLocalBranch name RefHead
    Git.checkout branch
    let cleanup = when shouldCreate $ Git.deleteBranch branch
    res <- act `catchError` (\e -> cleanup >> throwError e)
    cleanup
    return res
    where branch = LocalBranch name

transitionProposalToTarget :: Options -> Proposal -> Prefix -> PrepushLogs -> EShell ()
transitionProposalToTarget options proposal targetPrefix prepushLogs = do
    let targetProposalName = formatProposal $ proposal { proposalPrefix = Just targetPrefix }
        targetBranchName = mkBranchName targetProposalName
    eprint . T.pack $ "Creating target proposal branch: " <> T.unpack targetProposalName
    when (targetBranchName == ontoBranchName)
        $ abort $ "Can't handle branch, onto == target: " <> targetProposalName
    safeCreateBranch targetBranchName
    Git.checkout (LocalBranch ontoBranchName)
    Git.deleteLocalBranch targetBranchName
    sendProposalEmail options proposal ("Ran successfully, moved to: " <> prefixToText targetPrefix) "" (Just prepushLogs)
    return ()
    where
        ontoBranchName = proposalBranchOnto proposal

transitionProposalToCompletion :: Options -> Proposal -> PrepushLogs -> EShell ()
transitionProposalToCompletion options proposal prepushLogs = do
    Git.checkout (LocalBranch ontoBranchName)
    if isDryRun options proposal
    then sendProposalEmail options proposal "Dry-run: Prepush ran successfully" "" (Just prepushLogs)
    else do
        -- TODO ensure not dirty
        eprint $ "Updating: " <> fromBranchName ontoBranchName
        Git.push -- TODO -u origin master
        sendProposalEmail options proposal "Merged successfully" "" (Just prepushLogs)
    where
        ontoBranchName = proposalBranchOnto proposal

transitionProposal :: Options -> Proposal -> PrepushLogs -> EShell ()
transitionProposal options proposal prepushLogs =
    case optTargetPrefix options of
        Nothing -> transitionProposalToCompletion options proposal prepushLogs
        Just targetPrefix -> transitionProposalToTarget options proposal targetPrefix prepushLogs


attemptBranch :: IORef CurrentState -> Options -> FilePath -> Branch -> Proposal -> EShell ()
attemptBranch currentState options logDir proposalBranch proposal = do
    Git.fetch
    commits <- Git.log (proposalBranchBase proposal) (RefBranch proposalBranch)

    liftIO $ setStateProposal currentState proposal commits

    commitLogHtml <- proposalEmailHeader options proposal commits <$> Git.remoteUrl origin
    let title = if isDryRun options proposal
                then "Running dry run"
                else "Attempting to merge"
    sendProposalEmail options proposal title commitLogHtml Nothing

    -- cleanup leftover state from previous runs
    cleanupGit proposal

    remoteBranches <- Git.remoteBranches

    let niceBranchName = proposalName proposal
        niceBranch = LocalBranch niceBranchName
        ontoBranchName = proposalBranchOnto proposal
        remoteOnto = RefBranch $ RemoteBranch origin ontoBranchName
        verifyRemoteBranch rb =
            unless (rb `elem` remoteBranches)
            $ abort $ "No remote branch: " <> T.pack (show rb)

    verifyRemoteBranch (origin, ontoBranchName)

    -- note: 'nice' branch and 'onto' branch may be the same
    -- branch. (e.g. proposal called 'master' with onto=master)

    -- sync local onto with remote
    Git.checkout (LocalBranch ontoBranchName)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin ontoBranchName)
    finalBase <- Git.currentRef

    -- create local work branch, reset to proposed
    withLocalBranch niceBranchName $ do
        Git.reset Git.ResetHard (RefBranch proposalBranch)

        -- rebase work onto target
        Git.rebase Git.Rebase { Git.rebaseBase = proposalBranchBase proposal,
                                Git.rebaseOnto = remoteOnto,
                                Git.rebasePolicy = Git.RebaseDropMerges
                              }
            `catchError` rejectProposal options proposal "Rebase failed" Nothing

        eprint "Commits (after rebase): "
        commitsAfter <- Git.log (proposalBranchBase proposal) (RefBranch proposalBranch)
        liftIO $ mapM_ print commitsAfter

        -- rebase succeeded, we can now take this job

        let inProgressProposalName = formatProposal $ proposal { proposalStatus = ProposalInProgress }
            inProgressBranchName = mkBranchName inProgressProposalName
        eprint . T.pack $ "Creating in-progress proposal branch: " <> T.unpack inProgressProposalName

        Git.checkout proposalBranch

        withNewBranch inProgressBranchName $ do
            eprint "Deleting proposal branch..."
            Git.deleteBranch proposalBranch

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

            -- If this fails, reject branch will be created first; then the cleanup of in-progress
            -- branch will delete that one, so we're safe against losing the proposal
            let prepushCmd = cmdPrepushCommandAnArgs (optProposalMode options)
            runPrepush prepushLogs prepushCmd finalBase finalHead
                `catchError` rejectProposal options proposal "Prepush command failed" (Just prepushLogs)

            eprint "Prepush command ran succesfully"

            transitionProposal options proposal prepushLogs

            Git.checkout (LocalBranch ontoBranchName)
            Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin ontoBranchName)

            eprint $ "Finished handling proposal " <> formatProposal proposal

usage :: String
usage = List.intercalate "\n"
    [ "Usage: sling-server COMMAND"
    , ""
    , "where COMMAND is the prepush command to run on each attempted branch."
    ]

data PollMode = PollModeDaemon Int | PollModeOneShot | PollModeAllQueued

data PollOptions =
    PollOptions
    { optBranchFilterAll :: Maybe String
    , optBranchExcludeFilterAll :: Maybe String
    , optBranchFilterDryRun :: Maybe String
    , optBranchFilterNoDryRun :: Maybe String
    , optSourcePrefix :: Maybe Prefix
    , optPollMode :: PollMode
    }

data Options =
    Options
    { optWebServerPort :: Int
    , optEmailClient :: [Text]
    , optForceDryRun :: Bool
    , optTargetPrefix :: Maybe Prefix
    , optProposalMode :: Command
    }

data Command
    = Command { cmdMode :: PrepushMode
              , cmdPrepushCommandAnArgs :: [String] }

data PrepushMode
    = ProposalModePoll PollOptions
    | ProposalModeSingle Proposal

parseOpts :: IO Options
parseOpts = execParser $
    info (helper <*> parser)
    (fullDesc <> header "git-sling - merge branches with due process")

defaultEmailClient :: [Text]
defaultEmailClient = ["msmtp", "-C", "/opt/msmtp.conf"]

defaultPort :: Int
defaultPort = 8080

catchMaybe :: Maybe c -> c -> c
catchMaybe = flip fromMaybe

parseProposalFromCmdLine :: String -> Proposal
parseProposalFromCmdLine s =
    parseProposal (T.pack s)
    `catchMaybe` error ("Invalid proposal format: " ++ show s)

prepushCommandArgs :: Parser [String]
prepushCommandArgs =
    some (argument str
              (metavar "-- PREPUSH COMMAND LINE..." <>
               help "Pre-push command to run on each proposed branch (exit code 0 considered success)"))

parseModeBranches :: Parser Command
parseModeBranches =
    hsubparser
    ( command "proposal" (info
                          (Command <$>
                           (ProposalModeSingle .parseProposalFromCmdLine <$>
                               argument str (metavar "PROPOSAL" <>
                                             help "A proposal branch name to handle"))
                           <*> prepushCommandArgs)
                          (fullDesc <> progDesc "Process a single proposal"))
      <> command "poll" (info (Command
                                  <$> (ProposalModePoll <$> pollOptionsParser)
                                  <*> prepushCommandArgs)
                         (fullDesc <> progDesc "Process proposals queued as branches"))
    )
    -- flag' (ProposalFromBranch Nothing)
    --  (short 's' <>
    --   long "single" <>
    --   help "Process proposals from current (no polling) branches: Fetch current branches and process all existing proposals, then exit")
    -- <|> (ProposalFromBranch <$>
    --         optional (option auto
    --             (short 'd' <>
    --              metavar "T" <>
    --              long "daemon" <>
    --              help )))
    -- <|> (ProposalFromCommandLine . parseProposalFromCmdLine <$>
    --      strOption (long "proposal-branch" <>
    --                 short 'b' <>
    --                 help "A proposal branch name to handle"))

prefixOption :: Mod OptionFields String -> Parser Prefix
prefixOption args = prefixFromText . verify . T.pack <$> strOption args
    where
        verify t = if T.null t then error "Prefix can't be empty" else t

pollOptionsParser :: Parser PollOptions
pollOptionsParser =
    PollOptions
    <$> optional (strOption
                  (long "match-branches" <>
                   metavar "PATTERN" <>
                   help "Regex pattern to match 'onto' branch name in any proposal"))
    <*> optional (strOption
                  (long "exclude-branches" <>
                   metavar "PATTERN" <>
                   help "Regex pattern to exclude 'onto' branch name in any proposal"))
    <*> optional (strOption
                  (long "match-dry-run-branches" <>
                   metavar "PATTERN" <>
                   help "Regex pattern to match 'onto' branch name in dry run proposals"))
    <*> optional (strOption
                  (long "match-non-dry-run-branches" <>
                   metavar "PATTERN" <>
                   help "Regex pattern to match 'onto' branch name in non-dry run proposals"))
    <*> optional (prefixOption
                  (long "source-prefix" <>
                   help "Exact prefix of branches to be used for proposals (other proposals will be ignored)"))
    <*> (PollModeDaemon <$> (option auto $
                             short 'd' <>
                             metavar "T" <>
                             long "daemonize" <>
                             help "'Daemonize' - run endlessly, polling proposals from branches every T seconds")
         <|> (flag' PollModeOneShot
              (long "one-shot" <>
               help "Process one proposal and then quit; if nothing to process, quit immediately"))
         <|> (flag PollModeAllQueued PollModeAllQueued
              (long "all" <>
               help "(Default) Process all pending proposals and then quit; if nothing to process, quit immediately"))
        )


parser :: Parser Options
parser =
    Options
    <$> (option auto
          (value defaultPort <>
           short 'p' <>
           long "port" <>
           metavar "PORT" <>
           help ("Port for sling web server. Default: " <> show defaultPort)))
    <*> (fmap (maybe defaultEmailClient (T.words . T.pack)) <$>
         optional $ strOption
         (short 'e' <>
          long "email-client" <>
          metavar "EMAIL_CLIENT_COMMAND" <>
          help ("Command to use sending emails. Default: " <> T.unpack (T.intercalate " " defaultEmailClient))))
    <*> switch (short 'n' <>
                long "force-dry-run" <>
                help "Treat all proposals as dry run (regardless of what they say)")
    <*> optional (prefixOption
                  (long "target-prefix" <>
                   help "If missing, successful branches are merged (unless dry-run) & deleted. If non-empty, prefix of branches to be used for succesful proposals (branches will not be merged)"))
    <*> parseModeBranches


shouldConsiderProposal :: PollOptions -> Proposal -> Bool
shouldConsiderProposal pollOptions proposal =
    (ProposalProposed == proposalStatus proposal)
    && (optSourcePrefix pollOptions == proposalPrefix proposal)
    && fromMaybe True (checkFilter <$> optBranchFilterAll pollOptions)
    && fromMaybe True (not . checkFilter <$> optBranchExcludeFilterAll pollOptions)
    && fromMaybe True (((not $ proposalDryRun proposal) ||) . checkFilter <$> optBranchFilterDryRun pollOptions)
    && fromMaybe True ((proposalDryRun proposal ||) . checkFilter <$> optBranchFilterNoDryRun pollOptions)
    where checkFilter pat = (T.unpack . fromBranchName $ proposalBranchOnto proposal) =~ pat

handleSpecificProposal :: IORef CurrentState -> Options -> Proposal -> EShell ()
handleSpecificProposal state options proposal = do
    Git.fetch
    remoteBranches <- Git.remoteBranches
    let proposalBranchName = formatProposal proposal
        matchingBranches = filter (\b -> proposalBranchName == fromBranchName (snd b)) remoteBranches
    branch <- case matchingBranches of
        [] -> abort $ "Failed to find branch: " <> T.pack (show proposalBranchName)
        [b] -> return b
        _ -> abort $ "Assertion failed: multiple branches matching the same proposal: " <> T.pack (show proposalBranchName)
    attemptBranchOrAbort state options (uncurry Git.RemoteBranch branch) proposal

serverPoll :: IORef CurrentState -> Options -> PollOptions -> EShell Bool
serverPoll currentState options pollOptions = do
    Git.fetch
    remoteBranches <- Git.remoteBranches

    eprint . T.pack $ mconcat $ List.intersperse "\n\t"
        [ "Filters: "
        , maybe "" ("match filter: " <> ) (optBranchFilterAll pollOptions)
        , maybe "" ("exclude filter: " <> ) (optBranchExcludeFilterAll pollOptions)
        , maybe "" ("dry run branch filter: " <> ) (optBranchFilterDryRun pollOptions)
        , maybe "" ("non-dry-run branch filter: " <> ) (optBranchFilterNoDryRun pollOptions)
        ]
    eprint . T.pack $ mconcat $ List.intersperse "\n\t"
        [ "Prefixes: "
        , "Source: " <> maybe "" (T.unpack . prefixToText) (optSourcePrefix pollOptions)
        , "Target: " <> maybe "" (T.unpack . prefixToText) (optTargetPrefix options)
        ]

    let allProposals =
            List.sortOn (proposalQueueIndex . snd)
            $ mapMaybe ((\branch -> (branch,) <$> parseProposal (branchName branch))
                        . uncurry Git.RemoteBranch)
            remoteBranches
        proposals = filter (shouldConsiderProposal pollOptions . snd) allProposals

        showProposals ps = List.intercalate "\n\t" (map (show . branchName . fst) ps)

    eprint . T.pack $ "All proposals (before filtering):\n\t" <> showProposals allProposals
    eprint . T.pack $ "Going to attempt proposals:\n\t" <> showProposals proposals

    liftIO $ modifyIORef currentState $ \state -> state { csPendingProposals = map snd proposals }
    case proposals of
        [] -> do
            eprint "Done - have nothing to do."
            return False
        (topProposal:_) -> do
            (uncurry $ attemptBranchOrAbort currentState options) topProposal
            liftIO $ clearCurrentProposal currentState
            eprint . T.pack $ "Done proposal: " ++ show topProposal
            return True

serverLoop :: IORef CurrentState -> Options -> PollOptions -> EShell ()
serverLoop currentState options pollOptions = do
    void $ liftIO $ forkServer (optWebServerPort options) (readIORef currentState)
    let go = do
            havePending <- serverPoll currentState options pollOptions
            case (optPollMode pollOptions) of
                PollModeOneShot -> return ()
                PollModeAllQueued -> if havePending then go else return ()
                PollModeDaemon interval -> do
                    liftIO $ threadDelay $ interval*1000000
                    go
    go

main :: IO ()
main = runEShell $ eview $ do
    options <- liftIO parseOpts
    currentState <- liftIO $ newIORef emptyCurrentState
    case cmdMode (optProposalMode options) of
        ProposalModePoll pollOptions -> serverLoop currentState options pollOptions
        ProposalModeSingle proposal -> handleSpecificProposal currentState options proposal
