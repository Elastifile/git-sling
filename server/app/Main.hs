{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Monad (when, void, forM_)
import           Control.Monad.Except (MonadError (..))
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Sling.Git                     (Branch (..), Ref (..),
                                                Remote (..),
                                                mkBranchName)
import qualified Sling.Git as Git
import           Sling.Lib                     (EShell, abort, eproc,
                                                eprocsL, formatEmail, eprint,
                                                ignoreError, runEShell)
import qualified Sling as Sling
import           Sling.Options (parseOpts, isDryRun,
                                OptServerId(..), PrepushCmd(..), CommandType(..),
                                PrepushMode(..),
                                Options(..), PollOptions(..), PollMode(..),)
import           Sling.Path (encodeFP)
import           Sling.Prepush (PrepushLogs(..))
import           Sling.Proposal
import qualified Sling.Proposal as Proposal
import           Sling.Email (sendProposalEmail, EmailType(..))
import           Sling.Web (forkServer, CurrentState(..), emptyCurrentState)
import           Text.Regex.Posix ((=~))
import           Turtle (ExitCode, (&))

import qualified Data.List as List

import           Network.BSD (getHostName)

import qualified Filesystem.Path.CurrentOS as FP
import           Filesystem.Path.CurrentOS (FilePath)

import           System.IO.Temp (createTempDirectory)
import qualified Text.Blaze.Html5 as H
import           Control.Concurrent (threadDelay)
import           Options.Applicative

import           Prelude hiding (FilePath)

pollingInterval :: Int
pollingInterval = 1000000 * 10

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

attemptBranchOrAbort :: ServerId -> IORef CurrentState -> Options -> PrepushCmd -> Branch -> Proposal -> EShell ()
attemptBranchOrAbort serverId currentState options prepushCmd branch proposal = do
    cleanupBranches -- remove leftover branches
    Git.fetch
    -- cleanup leftover state from previous runs
    cleanupGit proposal

    job' <- Sling.tryTakeJob serverId options origin (branch, proposal)
    case job' of
        Nothing -> return () -- ignore
        Just job -> do
            commits <- Git.log (Sling.jobBase job) (Sling.jobHead job)
            setStateProposal currentState proposal commits

            dirPath <- FP.decodeString <$> liftIO (createTempDirectory "/tmp" "sling.log")
            runAttempt currentState options prepushCmd dirPath job
            `catchError` abortAttempt currentState options proposal

setStateProposal :: (Show a, Foldable t) => IORef CurrentState -> Proposal -> t a -> EShell ()
setStateProposal currentState proposal commits = do
    time <- liftIO $ getPOSIXTime
    liftIO $ modifyIORef currentState $ \state ->
        state
        { csCurrentProposal = Just (proposal, time)
        }
    eprint $ "Attempting proposal: " <> formatProposal proposal
    eprint "Commits: "
    mapM_ (eprint . T.pack . show) commits

safeCreateBranch :: Git.BranchName -> Git.PushType -> EShell ()
safeCreateBranch targetBranchName pushType = do
    Git.deleteLocalBranch targetBranchName & ignoreError
    _ <- Git.createLocalBranch targetBranchName RefHead
    _ <- Git.pushRemoteTracking origin targetBranchName pushType
    return ()

deleteLocalAndRemote :: Git.BranchName -> EShell ()
deleteLocalAndRemote b = do
    Git.deleteBranch (Git.LocalBranch b)
    Git.deleteBranch (RemoteBranch origin b)

transitionProposalToTarget :: Options -> Git.Ref -> Proposal -> Prefix -> PrepushLogs -> EShell ()
transitionProposalToTarget options newBase proposal targetPrefix prepushLogs = do
    newBaseHash <- Git.refToHash newBase
    shortBaseHash <- Git.shortenHash newBaseHash
    let updatedProposalType = case proposalType proposal of
            ProposalTypeMerge mergeType _oldBase -> ProposalTypeMerge mergeType shortBaseHash
            ProposalTypeRebase name -> ProposalTypeRebase name

        targetBranchName = Proposal.toBranchName $ proposal { proposalPrefix = Just targetPrefix
                                                            , proposalType = updatedProposalType
                                                            , proposalStatus = ProposalProposed }
    eprint . T.pack $ "Creating target proposal branch: " <> T.unpack (Git.fromBranchName targetBranchName)
    when (targetBranchName == ontoBranchName)
        $ abort $ "Can't handle branch, onto == target: " <> (Git.fromBranchName targetBranchName)
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
        case proposalType proposal of
            ProposalTypeMerge _mergeType _baseRef -> do
                eprint $ "Updating: " <> Git.fromBranchName ontoBranchName
                Git.checkout (RefBranch $ LocalBranch ontoBranchName)
                Git.push
            ProposalTypeRebase name  -> do
                eprint $ "Updating: " <> Git.fromBranchName name
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
    IORef CurrentState -> Options -> PrepushCmd -> FilePath -> Sling.Job -> EShell ()
runAttempt currentState options prepushCmd logDir (Sling.Job proposal inProgressBranchName finalBase finalHead) = do
    -- DO IT!
    logFileName <- head <$> eprocsL "mktemp" ["-p", encodeFP logDir, "prepush.XXXXXXX.txt"]
    let prepushLogs = PrepushLogs logDir (FP.fromText logFileName)

    liftIO $ modifyIORef currentState $ \state -> state { csCurrentLogFile = Just $ T.unpack logFileName }

    runPrepush prepushLogs prepushCmd finalBase finalHead
        `catchError` (Sling.rejectProposal options origin inProgressBranchName proposal "Prepush command failed" (Just prepushLogs) . Just)

    -- TODO ensure not dirty
    eprint "Prepush command ran succesfully"

    transitionProposal options finalBase finalHead proposal prepushLogs

    curHash <- Git.currentRefHash
    Git.checkout $ Git.RefHash curHash
    deleteLocalAndRemote inProgressBranchName

    eprint $ "Finished handling proposal " <> (formatProposal proposal)

usage :: String
usage = List.intercalate "\n"
    [ "Usage: sling-server COMMAND"
    , ""
    , "where COMMAND is the prepush command to run on each attempted branch."
    ]

shouldConsiderProposal :: PollOptions -> Proposal -> Bool
shouldConsiderProposal pollOptions proposal =
    (proposalStatus proposal /= ProposalRejected)
    && (optSourcePrefix pollOptions == proposalPrefix proposal)
    && fromMaybe True (checkFilter <$> optBranchFilterAll pollOptions)
    && fromMaybe True (not . checkFilter <$> optBranchExcludeFilterAll pollOptions)
    && fromMaybe True (((not $ proposalDryRun proposal) ||) . checkFilter <$> optBranchFilterDryRun pollOptions)
    && fromMaybe True ((proposalDryRun proposal ||) . checkFilter <$> optBranchFilterNoDryRun pollOptions)
    where checkFilter pat = (T.unpack . Git.fromBranchName $ proposalBranchOnto proposal) =~ pat

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
    $ mapMaybe (\branch -> (branch,) <$> Proposal.fromBranchName (Git.branchName branch))
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
        else return $ if (optInProgressFromAnyServer pollOptions)
                      then filteredProposals
                      else proposalsForThisServer

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

    let showProposals ps = List.intercalate "\n\t" (map (show . Git.branchName . fst) ps)

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
                eprint (Git.fromBranchName (Git.branchName branch) <> " " <> formatEmail (proposalEmail proposal))
