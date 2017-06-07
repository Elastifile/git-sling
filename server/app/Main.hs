{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Monad (when, unless, join, void, forM_)
import           Control.Monad.Except (MonadError (..))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import           Data.IORef
import           Data.Maybe (fromMaybe, mapMaybe, catMaybes)
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
import           Sling.Lib                     (EShell, Email (..), abort, eproc,
                                                eprocsIn, eprocsL, formatEmail, eprint,
                                                ignoreError, runEShell, fromHash)
import           Sling.Proposal
import           Sling.Web (forkServer, CurrentState(..), emptyCurrentState)
import           Text.Regex.Posix ((=~))
import           Turtle (ExitCode, (&), echo)

import qualified Data.List as List

import qualified Network.Mail.Mime as Mail
import           Network.BSD (getHostName)

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

pollingInterval :: Int
pollingInterval = 1000000 * 10

encodeFP :: FilePath -> T.Text
encodeFP = T.pack . BS8.unpack . FP.encode

runPrepush :: PrepushLogs -> PrepushCmd -> Ref -> Ref -> EShell ()
runPrepush (PrepushLogs logDir logFile) (PrepushCmd cmd) baseR headR = do
    let args = T.intercalate " " $ map T.pack cmd ++ [Git.refName baseR, Git.refName headR]
        env_str = "SLING_LOG_DIR=" <> encodeFP logDir
        bashArgs = [ "-o", "pipefail", "-c"
                   , " ( exec 2>&1; " <> env_str <> " " <> args
                     <> " ) | tee " <> encodeFP logFile]
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
        localOntoBranch = RefBranch $ LocalBranch ontoBranchName
        remoteOntoBranch = RefBranch $ RemoteBranch origin ontoBranchName
    localExists <- Git.exists localOntoBranch
    remoteExists <- Git.exists remoteOntoBranch
    when (localExists && remoteExists) $ do
        Git.checkout localOntoBranch
        Git.reset Git.ResetHard remoteOntoBranch

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

data IncludeAttachment = FullAttachment | MinimalAttachment
data EmailType = ProposalSuccessEmail | ProposalFailureEmail | ProposalAttemptEmail

addAttachments :: Maybe PrepushLogs -> IncludeAttachment -> Mail.Mail -> EShell Mail.Mail
addAttachments prepushLogs includeAttachment mail =
    case prepushLogs of
        Nothing -> return mail
        Just (PrepushLogs logDir fullLogOutputFile) -> do
            logPart <- liftIO $ addAttachment fullLogOutputFile "logtail.html"
            tarParts <- case includeAttachment of
                MinimalAttachment -> return []
                FullAttachment -> do
                    let tarName = encodeFP logDir <> ".tgz"
                    _tarOut <-
                        eprocsL "bash" ["-o", "pipefail", "-c",
                                        "tar czvf " <> tarName <> " " <> encodeFP logDir]
                    content <- liftIO $ LBS.readFile $ T.unpack tarName
                    return [Mail.Part "application/gzip" Mail.Base64 (Just tarName) [] content]
            return $ Mail.addPart (maybe id (:) logPart tarParts) mail


formatProposalEmail :: Options -> Proposal -> Text -> H.Html -> Maybe PrepushLogs -> IncludeAttachment -> EmailType -> EShell LBS.ByteString
formatProposalEmail options proposal subject body prepushLogs includeAttachment emailType = do
    webHref <- liftIO $ (<> (":" <> show (optWebServerPort options))) . ("http://" <>) <$> getHostName
    let html = renderHtml $ do
            H.p . H.b $ fromString $ T.unpack subject
            body
            case emailType of
                ProposalAttemptEmail -> H.p $ H.a H.! A.href (H.preEscapedToValue webHref)  $ "Sling Server Status"
                ProposalFailureEmail -> return ()
                ProposalSuccessEmail -> return ()

    mail1 <- liftIO $ Mail.simpleMail
        (Mail.Address Nothing $ formatEmail $ proposalEmail proposal)
        (Mail.Address Nothing $ formatEmail sourceEmail)
        ((if isDryRun options proposal then "(dry run) " else "")
         <> fromBranchName (proposalName proposal)
         <> " (" <> formatProposal proposal <> ")")
        ""
        html
        []

    mail <- case emailType of
        ProposalSuccessEmail -> return mail1
        ProposalAttemptEmail -> return mail1
        ProposalFailureEmail -> addAttachments prepushLogs includeAttachment mail1

    liftIO $ Mail.renderMail' mail

sendProposalEmail :: Options -> Proposal -> Text -> H.Html -> Maybe PrepushLogs -> EmailType -> EShell ()
sendProposalEmail options proposal subject body prepushLogs emailType = do
    let sendEmailWith includeAttachment = do
            renderdBS <- formatProposalEmail options proposal subject body prepushLogs includeAttachment emailType
            eprocsIn (head $ optEmailClient options) (tail (optEmailClient options) ++ [formatEmail $ proposalEmail proposal]) $ return (L.toStrict $ LE.decodeUtf8 renderdBS)

    eprint $ "Sending email (async) to: " <> formatEmail (proposalEmail proposal) <> " with subject: " <> subject
    liftIO $ void $ forkIO $ runEShell $ do
        sendEmailWith FullAttachment
        `catchError`
        \e -> do
                eprint $ "Error while sending with attachment, will attempt to send without attachment: " <> (T.pack $ show e)
                sendEmailWith MinimalAttachment

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
    sendProposalEmail options proposal "Aborting" (H.html $ H.text msg) Nothing ProposalFailureEmail
    cleanupGit proposal
    liftIO $ clearCurrentProposal currentState
    abort "Aborted"

slingBranchName :: Maybe Prefix -> Text -> Git.BranchName
slingBranchName Nothing suffix = mkBranchName suffix
slingBranchName (Just prefix) suffix = mkBranchName $ prefixToText prefix <> suffix

rejectProposal :: Options -> Branch -> Proposal -> Text -> Maybe PrepushLogs -> Maybe (Text, ExitCode) -> EShell ()
rejectProposal options proposalBranch proposal reason prepushLogs err = do
    let rejectedProposal = proposal { proposalStatus = ProposalRejected }
        rejectBranchName = formatProposal rejectedProposal
        origBranchName = Git.branchName proposalBranch
        suffix = case err of
            Just (msg, errCode) -> " because: '" <> reason <> "' (" <> msg <> "), exit code = " <> T.pack (show errCode)
            Nothing -> ""
        msgBody = "REJECT " <> origBranchName <> suffix
    eprint $ msgBody
    sendProposalEmail options proposal ("Rejecting (" <> reason <> ")") (toHtml msgBody) prepushLogs ProposalFailureEmail
    Git.fetch & ignoreError
    Git.deleteBranch (RemoteBranch origin $ mkBranchName rejectBranchName) & ignoreError -- in case it exists
    Git.deleteBranch (LocalBranch $ mkBranchName rejectBranchName) & ignoreError -- in case it exists
    Git.reset Git.ResetHard RefHead
    _ <- Git.createLocalBranch (mkBranchName rejectBranchName) RefHead
    _ <- Git.createRemoteTrackingBranch origin (mkBranchName rejectBranchName) Git.PushForceWithoutLease
    -- We have to be on another branch before deleting stuff, so arbitrarily picking rejected branch
    Git.checkout (Git.RefBranch . LocalBranch $ mkBranchName rejectBranchName)
    Git.deleteBranch (LocalBranch . mkBranchName $ formatProposal proposal) & ignoreError
    Git.deleteBranch (RemoteBranch origin $ mkBranchName origBranchName)
    abort "Rejected"

attemptBranchOrAbort :: ServerId -> IORef CurrentState -> Options -> PrepushCmd -> Branch -> Proposal -> EShell ()
attemptBranchOrAbort serverId currentState options prepushCmd branch proposal = do
    dirPath <- FP.decodeString <$> liftIO (createTempDirectory "/tmp" "sling.log")
    attemptBranch serverId currentState options prepushCmd dirPath branch proposal `catchError` abortAttempt currentState options proposal

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

safeCreateBranch :: Git.BranchName -> Git.PushType -> EShell ()
safeCreateBranch targetBranchName pushType = do
    Git.deleteLocalBranch targetBranchName & ignoreError
    _ <- Git.createLocalBranch targetBranchName RefHead
    _ <- Git.createRemoteTrackingBranch origin targetBranchName pushType
    return ()

deleteLocalAndRemote :: Git.BranchName -> EShell ()
deleteLocalAndRemote b = do
    Git.deleteBranch (Git.LocalBranch b)
    Git.deleteBranch (RemoteBranch origin b)

withNewBranch :: Git.BranchName -> Git.PushType -> EShell a -> EShell a
withNewBranch b pushType act = do
    currentRef <- Git.currentRef
    safeCreateBranch b pushType
    let cleanup = do
            Git.checkout currentRef
            deleteLocalAndRemote b
    res <- act `catchError` (\e -> cleanup >> throwError e)
    cleanup
    return res

withLocalBranch :: Git.BranchName -> EShell () -> EShell ()
withLocalBranch name act = do
    currentRef <- Git.currentRef
    Git.deleteBranch branch & ignoreError
    Git.localBranches >>= (liftIO . mapM_ print)
    shouldCreate <- not . elem name <$> Git.localBranches
    when shouldCreate $ void $ Git.createLocalBranch name RefHead
    Git.checkout (RefBranch branch)
    let cleanup = do
            Git.checkout currentRef
            when shouldCreate $ Git.deleteBranch branch
    res <- act `catchError` (\e -> cleanup >> throwError e)
    cleanup
    return res
    where branch = LocalBranch name

transitionProposalToTarget :: Options -> Git.Ref -> Proposal -> Prefix -> PrepushLogs -> EShell ()
transitionProposalToTarget options newBase proposal targetPrefix prepushLogs = do
    Git.RefHash shortBaseHash <- Git.shortenRef newBase
    let moveBranch = case proposalMove proposal of
                       MoveBranchOnto mergeType _oldBase -> MoveBranchOnto mergeType shortBaseHash
                       MoveBranchProposed name -> MoveBranchProposed name

        targetProposalName = formatProposal $ proposal { proposalPrefix = Just targetPrefix
                                                       , proposalMove = moveBranch
                                                       , proposalStatus = ProposalProposed }
        targetBranchName = mkBranchName targetProposalName
    eprint . T.pack $ "Creating target proposal branch: " <> T.unpack targetProposalName
    when (targetBranchName == ontoBranchName)
        $ abort $ "Can't handle branch, onto == target: " <> targetProposalName
    safeCreateBranch targetBranchName Git.PushNonForce
    Git.checkout (RefBranch $ LocalBranch ontoBranchName)
    Git.deleteLocalBranch targetBranchName
    sendProposalEmail options proposal ("Ran successfully, moved to: " <> prefixToText targetPrefix) "" (Just prepushLogs) ProposalSuccessEmail
    return ()
    where
        ontoBranchName = proposalBranchOnto proposal

transitionProposalToCompletion :: Options -> Git.Ref -> Proposal -> PrepushLogs -> EShell ()
transitionProposalToCompletion options finalHead proposal prepushLogs = do
    if isDryRun options proposal
    then sendProposalEmail options proposal "Dry-run: Prepush ran successfully" "" (Just prepushLogs) ProposalSuccessEmail
    else do
        case proposalMove proposal of
            MoveBranchOnto _mergeType _baseRef -> do
                eprint $ "Updating: " <> fromBranchName ontoBranchName
                Git.checkout (RefBranch $ LocalBranch ontoBranchName)
                Git.push
            MoveBranchProposed name  -> do
                eprint $ "Updating: " <> fromBranchName name
                Git.deleteLocalBranch name & ignoreError
                Git.checkout (RefBranch $ LocalBranch name)
                Git.reset Git.ResetHard finalHead
                Git.pushForceWithLease

        sendProposalEmail options proposal "Merged successfully" "" (Just prepushLogs) ProposalSuccessEmail
    where
        ontoBranchName = proposalBranchOnto proposal

transitionProposal :: Options -> Git.Ref -> Git.Ref -> Proposal -> PrepushLogs -> EShell ()
transitionProposal options finalBase finalHead proposal prepushLogs =
    case optTargetPrefix options of
        Nothing -> transitionProposalToCompletion options finalHead proposal prepushLogs
        Just targetPrefix -> transitionProposalToTarget options finalBase proposal targetPrefix prepushLogs

runAttempt ::
    IORef CurrentState -> Options -> PrepushCmd -> FilePath -> Branch -> Proposal ->
    Ref -> Ref -> Git.BranchName -> EShell ()
runAttempt currentState options prepushCmd logDir origBranchName proposal finalBase finalHead ontoBranchName = do

    -- DO IT!
    logFileName <- head <$> eprocsL "mktemp" ["-p", encodeFP logDir, "prepush.XXXXXXX.txt"]
    let prepushLogs = PrepushLogs logDir (FP.fromText logFileName)
    liftIO $ modifyIORef currentState $ \state -> state { csCurrentLogFile = Just $ T.unpack logFileName }

    -- If this fails, reject branch will be created first; then the cleanup of in-progress
    -- branch will delete that one, so we're safe against losing the proposal
    runPrepush prepushLogs prepushCmd finalBase finalHead
        `catchError` (rejectProposal options origBranchName proposal "Prepush command failed" (Just prepushLogs) . Just)

    -- TODO ensure not dirty
    eprint "Prepush command ran succesfully"

    transitionProposal options finalBase finalHead proposal prepushLogs

    Git.checkout (RefBranch $ LocalBranch ontoBranchName)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin ontoBranchName)

    eprint $ "Finished handling proposal " <> formatProposal proposal


attemptBranch :: ServerId -> IORef CurrentState -> Options -> PrepushCmd -> FilePath -> Branch -> Proposal -> EShell ()
attemptBranch serverId currentState options prepushCmd logDir proposalBranch proposal = do
    cleanupBranches -- remove leftover branches
    Git.fetch
    let ontoBranchName = proposalBranchOnto proposal
        remoteOnto = RefBranch $ RemoteBranch origin ontoBranchName
        (baseRef, headRef) =
            case proposalMove proposal of
              MoveBranchOnto _mergeType base -> (Git.RefHash base, RefBranch proposalBranch)
              MoveBranchProposed name -> (remoteOnto, Git.RefBranch $ Git.RemoteBranch origin name)

    -- cleanup leftover state from previous runs
    cleanupGit proposal

    remoteBranches <- Git.remoteBranches

    let niceBranchName = proposalName proposal
        niceBranch = LocalBranch niceBranchName
        verifyRemoteBranch :: (Git.Remote, Git.BranchName) -> EShell ()
        verifyRemoteBranch rb =
            unless (rb `elem` remoteBranches)
            $ rejectProposal options proposalBranch proposal ("Remote branch doesn't exist: " <> fromBranchName ontoBranchName) Nothing Nothing

    verifyRemoteBranch (origin, ontoBranchName)

    commits <- Git.log baseRef headRef -- must be done after we verify the remote branch exists

    liftIO $ setStateProposal currentState proposal commits

    commitLogHtml <- proposalEmailHeader options proposal commits <$> Git.remoteUrl origin
    let title = if isDryRun options proposal
                then "Running dry run"
                else "Attempting to merge"

    -- note: 'nice' branch and 'onto' branch may be the same
    -- branch. (e.g. proposal called 'master' with onto=master)

    -- sync local onto with remote
    Git.checkout (RefBranch $ LocalBranch ontoBranchName)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin ontoBranchName)
    finalBaseHash <- Git.currentRefHash
    let finalBase = Git.RefHash finalBaseHash

    -- create local work branch, reset to proposed
    withLocalBranch niceBranchName $ do
        Git.reset Git.ResetHard headRef

        -- rebase work onto target
        Git.rebase Git.Rebase { Git.rebaseBase = baseRef,
                                Git.rebaseOnto = remoteOnto,
                                Git.rebasePolicy =
                                      case proposalMove proposal of
                                          MoveBranchOnto MergeTypeFlat       _ -> Git.RebaseDropMerges
                                          MoveBranchOnto MergeTypeKeepMerges _ -> Git.RebaseKeepMerges
                                          MoveBranchProposed{}                 -> Git.RebaseKeepMerges
                              }
            `catchError` (rejectProposal options proposalBranch proposal "Rebase failed" Nothing . Just)

        -- rebase succeeded, we can now take this job

        case proposalMove proposal of
            MoveBranchOnto{} -> do
              isMerge <- Git.isMergeCommit (RefBranch niceBranch)
              let mergeFF =
                      if isMerge || (length commits == 1)
                      then Git.MergeFFOnly
                      else Git.MergeNoFF
              -- go back to 'onto', decide whether to create a merge commit on
              -- top (if we should merge ff only)
              Git.checkout (RefBranch $ LocalBranch ontoBranchName)
              Git.merge mergeFF niceBranch
              when (mergeFF == Git.MergeNoFF) $
                  Git.commitAmend (proposalEmail proposal) Git.RefHead
              newHead <- Git.currentRef
              -- Fast-forward the work branch to match the merged 'onto' we do
              -- this so that the prepush script will see itself running on a
              -- branch with the name the user gave to this proposal, and not
              -- the onto branch's name.
              Git.checkout (RefBranch niceBranch)
              Git.reset Git.ResetHard newHead

            MoveBranchProposed{} -> return () -- do nothing

        finalHead <- Git.currentRef

        eprint "Switching to (new) in-progress branch"
        let forceCreateInProgress = case proposalStatus proposal of
                ProposalInProgress{} -> Git.PushForceWithoutLease -- can't use lease to create new branch. stupid git.
                _                    -> Git.PushNonForce

        let newProposalMove = case proposalMove proposal of
                MoveBranchOnto mergeType _baseHash -> MoveBranchOnto mergeType finalBaseHash
                MoveBranchProposed moveBranchName -> MoveBranchProposed moveBranchName

            inProgressProposalName = formatProposal $ proposal { proposalStatus = ProposalInProgress serverId
                                                               , proposalMove = newProposalMove }
            inProgressBranchName = mkBranchName inProgressProposalName
        eprint . T.pack $ "Creating in-progress proposal branch: " <> T.unpack inProgressProposalName

        withNewBranch inProgressBranchName forceCreateInProgress $ do
            eprint "Deleting proposal branch..."
            jobTaken <- case proposalStatus proposal of
                ProposalInProgress{} -> return True
                ProposalProposed{} -> (Git.deleteBranch proposalBranch >> return True)
                    `catchError` (const $ eprint "Can't delete proposal - Other slave took the job? Dropping" >> return False)
                ProposalRejected -> error "ASSERTION FAILED! Shouldn't be taking rejected proposal"
            when jobTaken $ do
                sendProposalEmail options proposal title commitLogHtml Nothing ProposalAttemptEmail
                runAttempt currentState options prepushCmd logDir proposalBranch proposal finalBase finalHead ontoBranchName

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
    , optNoConcurrent :: Bool
    }


data OptServerId = OptServerIdHostName | OptServerIdName Text

data Options =
    Options
    { optWebServerPort :: Int
    , optEmailClient :: [Text]
    , optForceDryRun :: Bool
    , optTargetPrefix :: Maybe Prefix
    , optServerId :: OptServerId
    , optCommandType :: CommandType
    }

data PrepushCmd = PrepushCmd [String]

data CommandType
    = CommandTypePropose { _cmdMode :: PrepushMode
                         , _cmdPrepushCommandAnArgs :: PrepushCmd }
    | CommandTypeList PollOptions

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

prepushCommandArgs :: Parser PrepushCmd
prepushCommandArgs = PrepushCmd <$>
    some (argument str
              (metavar "-- PREPUSH COMMAND LINE..." <>
               help "Pre-push command to run on each proposed branch (exit code 0 considered success)"))

parseModeBranches :: Parser CommandType
parseModeBranches =
    hsubparser
    ( command "proposal" (info
                          (CommandTypePropose <$>
                           (ProposalModeSingle .parseProposalFromCmdLine <$>
                               argument str (metavar "PROPOSAL" <>
                                             help "A proposal branch name to handle"))
                           <*> prepushCommandArgs)
                          (fullDesc <> progDesc "Process a single proposal"))
      <> command "list" (info (CommandTypeList <$> pollOptionsParser)
                         (fullDesc <> progDesc "List pending proposals"))
      <> command "poll" (info (CommandTypePropose
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
    <*> switch ( long "no-concurrent" <>
                  help "Prevent concurrent jobs: don't match any proposal, if there's an in-progress proposal matching the filter" )


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
                   help "ITypef missing, successful branches are merged (unless dry-run) & deleted. If non-empty, prefix of branches to be used for succesful proposals (branches will not be merged)"))
    <*> (fmap (maybe OptServerIdHostName (OptServerIdName . T.pack)) <$>
         optional $ strOption
         (long "server-id" <>
          metavar "SERVER_ID" <>
          help ("Used for resuming in-progress jobs that were aborted due to a failed server")))
    <*> parseModeBranches


shouldConsiderProposal :: PollOptions -> Proposal -> Bool
shouldConsiderProposal pollOptions proposal =
    (optSourcePrefix pollOptions == proposalPrefix proposal)
    && fromMaybe True (checkFilter <$> optBranchFilterAll pollOptions)
    && fromMaybe True (not . checkFilter <$> optBranchExcludeFilterAll pollOptions)
    && fromMaybe True (((not $ proposalDryRun proposal) ||) . checkFilter <$> optBranchFilterDryRun pollOptions)
    && fromMaybe True ((proposalDryRun proposal ||) . checkFilter <$> optBranchFilterNoDryRun pollOptions)
    where checkFilter pat = (T.unpack . fromBranchName $ proposalBranchOnto proposal) =~ pat

handleSpecificProposal :: ServerId -> IORef CurrentState -> Options -> PrepushCmd -> Proposal -> EShell ()
handleSpecificProposal serverId state options prepushCmd proposal = do
    Git.fetch
    allProposals <- getProposals
    let matchingBranches = filter (\(_b, p) -> proposal == p) allProposals
    branch <- case matchingBranches of
        [] -> abort $ "Failed to find branch for proposal: " <> (formatProposal proposal)
        [(b, _p)] -> return b
        _ -> abort $ "Assertion failed: multiple branches matching the same proposal: " <> (formatProposal proposal)
    attemptBranchOrAbort serverId state options prepushCmd branch proposal

parseProposals :: [Branch] -> [(Branch, Proposal)]
parseProposals remoteBranches =
    List.sortOn (proposalQueueIndex . snd)
    $ mapMaybe (\branch -> (branch,) <$> parseProposal (branchName branch))
    remoteBranches

getProposals :: EShell [(Branch, Proposal)]
getProposals = parseProposals . map (uncurry Git.RemoteBranch) <$> Git.remoteBranches

getFilteredProposals :: ServerId -> PollOptions -> EShell [(Branch, Proposal)]
getFilteredProposals serverId pollOptions = do
    allProposals <- getProposals
    let filteredProposals = filter (shouldConsiderProposal pollOptions . snd) allProposals

        forThisServer proposal =
            case proposalStatus proposal of
                ProposalProposed{} -> True
                ProposalInProgress proposalServerId -> proposalServerId == serverId
                ProposalRejected -> False
        proposalsForThisServer = filter (forThisServer . snd) filteredProposals

        isOnOtherServer proposal =
            case proposalStatus proposal of
                ProposalProposed{} -> False
                ProposalInProgress proposalServerId -> proposalServerId /= serverId
                ProposalRejected -> False

    if (optNoConcurrent pollOptions) && (any (isOnOtherServer . snd) filteredProposals)
        then return []
        else return proposalsForThisServer

cleanupBranches :: EShell ()
cleanupBranches = do
    slingLocalBranches <- map fst
        . catMaybes
        . map (\b -> fmap (b,) . parseProposal . Git.fromBranchName $ b)
        <$> Git.localBranches
    mapM_ (\x -> (Git.deleteLocalBranch x) & ignoreError) slingLocalBranches

serverPoll :: ServerId -> IORef CurrentState -> Options -> PrepushCmd -> PollOptions -> EShell Bool
serverPoll serverId currentState options prepushCmd pollOptions = do
    allProposals <- getProposals
    proposals <- getFilteredProposals serverId pollOptions

    eprint . T.pack $ mconcat $ List.intersperse "\n\t"
        [ "Filters: "
        , maybe "" ("match filter: " <> ) (optBranchFilterAll pollOptions)
        , maybe "" ("exclude filter: " <> ) (optBranchExcludeFilterAll pollOptions)
        , maybe "" ("dry run branch filter: " <> ) (optBranchFilterDryRun pollOptions)
        , maybe "" ("non-dry-run branch filter: " <> ) (optBranchFilterNoDryRun pollOptions)
        ]

    eprint . T.pack $ "Allow concurrent: " <> if optNoConcurrent pollOptions then "No" else "Yes"

    eprint . T.pack $ mconcat $ List.intersperse "\n\t"
        [ "Prefixes: "
        , "Source: " <> maybe "" (T.unpack . prefixToText) (optSourcePrefix pollOptions)
        , "Target: " <> maybe "" (T.unpack . prefixToText) (optTargetPrefix options)
        ]

    let showProposals ps = List.intercalate "\n\t" (map (show . branchName . fst) ps)

    eprint . T.pack $ "All proposals (before filtering):\n\t" <> showProposals allProposals
    eprint . T.pack $ "Going to attempt proposals:\n\t" <> showProposals proposals

    liftIO $ modifyIORef currentState $ \state -> state { csPendingProposals = map snd proposals }
    case proposals of
        [] -> do
            eprint "Done - have nothing to do."
            return False
        (topProposal:_) -> do
            (uncurry $ attemptBranchOrAbort serverId currentState options prepushCmd) topProposal
            liftIO $ clearCurrentProposal currentState
            eprint . T.pack $ "Done proposal: " ++ show topProposal
            return True

serverLoop :: ServerId -> IORef CurrentState -> Options -> PrepushCmd -> PollOptions -> EShell ()
serverLoop serverId currentState options prepushCmd pollOptions = do
    void $ liftIO $ forkServer (optWebServerPort options) (readIORef currentState)
    let go = do
            havePending <- serverPoll serverId currentState options prepushCmd pollOptions
            case (optPollMode pollOptions) of
                PollModeOneShot -> return ()
                PollModeAllQueued -> if havePending then go else return ()
                PollModeDaemon interval -> do
                    liftIO $ threadDelay $ interval*1000000
                    go
    go

main :: IO ()
main = runEShell $ do
    options <- liftIO parseOpts
    currentState <- liftIO $ newIORef emptyCurrentState
    hostName <- liftIO getHostName
    let serverId = ServerId $ case optServerId options of
            OptServerIdHostName -> T.pack hostName
            OptServerIdName serverIdName -> serverIdName

    Git.fetch
    case optCommandType options of
        CommandTypePropose (ProposalModePoll pollOptions) prepushCmd -> serverLoop serverId currentState options prepushCmd pollOptions
        CommandTypePropose (ProposalModeSingle proposal) prepushCmd -> handleSpecificProposal serverId currentState options prepushCmd proposal
        CommandTypeList pollOptions -> do
            proposals <- getFilteredProposals serverId pollOptions
            forM_ proposals $ \(branch, proposal) -> do
                eprint (branchName branch <> " " <> formatEmail (proposalEmail proposal))
