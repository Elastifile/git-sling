{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Monad (when, void, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Sling.Git                     (Branch (..), Ref (..),
                                                Remote (..),
                                                mkBranchName)
import qualified Sling.Git as Git
import           Sling.Lib                     (EShell, abort, Hash(..),
                                                eprocsL, formatEmail, eprint,
                                                ignoreError, runEShell)
import qualified Sling
import           Sling.Options (parseOpts,
                                OptServerId(..), PrepushCmd(..), CommandType(..),
                                PrepushMode(..), FilterOptions(..),
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

import           System.IO.Temp (createTempDirectory)
import qualified Text.Blaze.Html5 as H
import           Control.Concurrent (threadDelay)
import           Options.Applicative

import           Prelude hiding (FilePath)

pollingInterval :: Int
pollingInterval = 1000000 * 10

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

cleanupGit :: Proposal -> EShell ()
cleanupGit proposal = do
    Git.rebaseAbort & ignoreError
    Git.reset Git.ResetHard RefHead
    resetLocalOnto proposal

----------------------------------------------------------------------

clearCurrentProposal :: IORef CurrentState -> IO ()
clearCurrentProposal currentState =
    modifyIORef currentState $ \state ->
        state
        { csCurrentProposal = Nothing
        , csCurrentLogFile = Nothing }

setStateProposal :: (Show a, Foldable t) => IORef CurrentState -> Proposal -> t a -> EShell ()
setStateProposal currentState proposal commits = do
    time <- liftIO getPOSIXTime
    liftIO $ modifyIORef currentState $ \state ->
        state
        { csCurrentProposal = Just (proposal, time)
        }
    eprint $ "Attempting proposal: " <> formatProposal proposal
    eprint "Commits: "
    mapM_ (eprint . T.pack . show) commits

setCurrentLogFile :: IORef CurrentState -> Text -> EShell ()
setCurrentLogFile currentState logFileName = liftIO $ modifyIORef currentState $ \state -> state { csCurrentLogFile = Just $ T.unpack logFileName }

setCurrentJob :: IORef CurrentState -> Text -> Sling.Job -> EShell ()
setCurrentJob currentState logFileName (Sling.Job proposal baseRef headRef) = do
    commits <- Git.log baseRef headRef
    setStateProposal currentState proposal commits
    setCurrentLogFile currentState logFileName

----------------------------------------------------------------------

abortAttempt :: IORef CurrentState -> Options -> Proposal -> (Text, ExitCode) -> EShell a
abortAttempt currentState options proposal (msg, _err) = do
    eprint . T.pack $ "ABORTING: " ++ show msg
    sendProposalEmail options proposal "Aborting" (H.html $ H.text msg) Nothing ProposalFailureEmail
    cleanupGit proposal
    liftIO $ clearCurrentProposal currentState
    abort "Aborted"

slingBranchName :: Maybe Prefix -> Text -> Git.BranchName
slingBranchName Nothing suffix = mkBranchName suffix
slingBranchName (Just prefix) suffix = mkBranchName $ prefixToText prefix <> suffix

----------------------------------------------------------------------

tryTakeJob :: ServerId -> Options -> (Branch, Proposal) -> EShell (Maybe Sling.Job)
tryTakeJob serverId options (branch, proposal) = do
    cleanupBranches -- remove leftover branches
    Git.fetch
    -- cleanup leftover state from previous runs
    cleanupGit proposal

    Sling.tryTakeJob serverId options origin (branch, proposal)

attemptBranchOrAbort :: ServerId -> IORef CurrentState -> Options -> PrepushCmd -> Branch -> Proposal -> EShell ()
attemptBranchOrAbort serverId currentState options prepushCmd branch proposal = do
    job' <- tryTakeJob serverId options (branch, proposal)
    case job' of
        Nothing -> return () -- ignore
        Just job -> do

            dirPath <- FP.decodeString <$> liftIO (createTempDirectory "/tmp" "sling.log")
            logFileName <- head <$> eprocsL "mktemp" ["-p", encodeFP dirPath, "prepush.XXXXXXX.txt"]
            let prepushLogs = PrepushLogs dirPath (FP.fromText logFileName)

            setCurrentJob currentState logFileName job

            -- DO IT!
            Sling.runPrepush options origin prepushCmd prepushLogs job

            Sling.transitionProposal options origin job (Just prepushLogs)
            eprint $ "Finished handling proposal " <> formatProposal proposal

----------------------------------------------------------------------

usage :: String
usage = List.intercalate "\n"
    [ "Usage: sling-server COMMAND"
    , ""
    , "where COMMAND is the prepush command to run on each attempted branch."
    ]

shouldConsiderProposal :: FilterOptions -> Proposal -> Bool
shouldConsiderProposal filterOptions proposal =
    (proposalStatus proposal /= ProposalRejected)
    && (optSourcePrefix filterOptions == proposalPrefix proposal)
    && fromMaybe True (checkFilter <$> optBranchFilterAll filterOptions)
    && fromMaybe True (not . checkFilter <$> optBranchExcludeFilterAll filterOptions)
    && fromMaybe True (((not $ proposalDryRun proposal) ||) . checkFilter <$> optBranchFilterDryRun filterOptions)
    && fromMaybe True ((proposalDryRun proposal ||) . checkFilter <$> optBranchFilterNoDryRun filterOptions)
    where checkFilter pat = (T.unpack . Git.fromBranchName $ proposalBranchOnto proposal) =~ pat

handleSpecificProposal :: ServerId -> IORef CurrentState -> Options -> PrepushCmd -> Proposal -> EShell ()
handleSpecificProposal serverId state options prepushCmd proposal = do
    Git.fetch
    allProposals <- getProposals
    let matchingBranches = filter (\(_b, p) -> proposal == p) allProposals
    branch <- case matchingBranches of
        [] -> abort $ "Failed to find branch for proposal: " <> formatProposal proposal
        [(b, _p)] -> return b
        _ -> abort $ "Assertion failed: multiple branches matching the same proposal: " <> formatProposal proposal
    attemptBranchOrAbort serverId state options prepushCmd branch proposal

parseProposals :: [Branch] -> [(Branch, Proposal)]
parseProposals remoteBranches =
    List.sortOn (proposalQueueIndex . snd)
    $ mapMaybe (\branch -> (branch,) <$> Proposal.fromBranchName (Git.branchName branch))
    remoteBranches

getProposals :: EShell [(Branch, Proposal)]
getProposals = parseProposals . map (uncurry Git.RemoteBranch) <$> Git.remoteBranches

getFilteredProposals :: ServerId -> FilterOptions -> EShell [(Branch, Proposal)]
getFilteredProposals serverId filterOptions = do
    allProposals <- getProposals
    let filteredProposals = filter (shouldConsiderProposal filterOptions . snd) allProposals

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

    if optNoConcurrent filterOptions && any (isOnOtherServer . snd) filteredProposals
        then return []
        else return $ if optInProgressFromAnyServer filterOptions
                      then filteredProposals
                      else proposalsForThisServer

cleanupBranches :: EShell ()
cleanupBranches = do
    slingLocalBranches <- map fst
        . mapMaybe (\b -> fmap (b,) . parseProposal . Git.fromBranchName $ b)
        <$> Git.localBranches
    mapM_ (\x -> Git.deleteLocalBranch x & ignoreError) slingLocalBranches

serverPoll :: ServerId -> IORef CurrentState -> Options -> PrepushCmd -> PollOptions -> EShell Bool
serverPoll serverId currentState options prepushCmd pollOptions = do
    let filterOptions = optFilterOptions pollOptions
    allProposals <- getProposals
    proposals <- getFilteredProposals serverId filterOptions

    eprint . T.pack $ mconcat $ List.intersperse "\n\t"
        [ "Filters: "
        , maybe "" ("match filter: " <> ) (optBranchFilterAll filterOptions)
        , maybe "" ("exclude filter: " <> ) (optBranchExcludeFilterAll filterOptions)
        , maybe "" ("dry run branch filter: " <> ) (optBranchFilterDryRun filterOptions)
        , maybe "" ("non-dry-run branch filter: " <> ) (optBranchFilterNoDryRun filterOptions)
        ]

    eprint . T.pack $ "Allow concurrent: " <> if optNoConcurrent filterOptions then "No" else "Yes"

    eprint . T.pack $ mconcat $ List.intersperse "\n\t"
        [ "Prefixes: "
        , "Source: " <> maybe "" (T.unpack . prefixToText) (optSourcePrefix filterOptions)
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
            case optPollMode pollOptions of
                PollModeOneShot -> return ()
                PollModeAllQueued -> when havePending go
                PollModeDaemon interval -> do
                    liftIO $ threadDelay $ interval*1000000
                    go
    go

notInProgress :: Proposal -> Bool
notInProgress proposal = case Proposal.proposalStatus proposal of
    Proposal.ProposalInProgress{} -> False
    _ -> True

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
            forM_ proposals $ \(branch, proposal) ->
                liftIO $ putStrLn $ T.unpack $ (Git.fromBranchName (Git.branchName branch) <> " " <> formatEmail (proposalEmail proposal))
        CommandTypeRebase pollOptions -> do
            proposals <- filter (notInProgress . snd) <$> getFilteredProposals serverId pollOptions
            forM_ proposals $ Sling.updateProposal options origin
        CommandTypeTakeJob pollOptions -> do
            proposals <- filter (notInProgress . snd) <$> getFilteredProposals serverId pollOptions
            case proposals of
                (topProposal:_) -> do
                    mjob <- tryTakeJob serverId options topProposal
                    case mjob of
                        Nothing -> return ()
                        Just (Sling.Job proposal baseRef headRef) -> do
                            baseHash <- Git.refToHash baseRef
                            headHash <- Git.refToHash headRef
                            liftIO $ mapM_ putStrLn $
                                [ T.unpack (Proposal.formatProposal proposal)
                                , T.unpack $ fromHash baseHash
                                , T.unpack $ fromHash headHash
                                ]
                _ -> return ()
        CommandTypeTransition proposal -> do
            case Proposal.proposalType proposal of
                Proposal.ProposalTypeRebase{} -> abort "Can't transition a rebase proposal"
                Proposal.ProposalTypeMerge _mergeType baseHash -> do
                    headRef <- Git.RefHash <$> Git.refToHash (Git.RefBranch $ Git.RemoteBranch origin (Proposal.toBranchName proposal))
                    Sling.transitionProposal options origin (Sling.Job proposal (Git.RefHash baseHash) headRef) Nothing
        CommandTypeReject proposal reason -> do
            Sling.rejectProposal options origin proposal reason Nothing Nothing
