{-# LANGUAGE OverloadedStrings #-}
module Sling
    ( Job(..)
    , tryTakeJob
    , rejectProposal
    , updateProposal
    , transitionProposal
    , runPrepush
    ) where

import           Control.Monad (unless, when, void)
import           Control.Monad.Except (MonadError (..))
import           Data.Monoid ((<>))
import           Data.Text              (Text)
import qualified Data.Text as T
import           Text.Blaze.Html (toHtml)
import           Turtle ((&), ExitCode)

import qualified Sling.Git as Git
import           Sling.Email (sendProposalEmail, formatCommitsForEmail, EmailType(..))
import           Sling.Lib (EShell, Hash(..), ignoreError, assert, eprint, abort, eproc)
import           Sling.Options (Options, isDryRun)
import qualified Sling.Options as Options
import           Sling.Path (encodeFP)
import           Sling.Prepush (PrepushLogs(..))
import qualified Sling.Proposal as Proposal
import           Sling.Proposal (Proposal)



data Job
    = Job
    { jobProposal :: Proposal
    , jobBase :: Git.Ref
    , jobHead :: Git.Ref
    } deriving (Show)

makeUnique :: Proposal -> EShell Proposal
makeUnique proposal = do
    remoteBranches <- map snd <$> Git.remoteBranches
    let proposalBranch = Proposal.toBranchName proposal
    if proposalBranch `elem` remoteBranches
        then do
        let oldName = Git.fromBranchName $ Proposal.proposalName proposal
            withSuffix sfx = proposal { Proposal.proposalName = Git.mkBranchName $ oldName <> "_" <> T.pack (show sfx) }
            go n = if Proposal.toBranchName newProposal `elem` remoteBranches
                   then go (n+1)
                   else newProposal
                where newProposal = withSuffix n
        return $ go (1 :: Int)
        else return proposal

rejectProposalAndAbort :: Options -> Git.Remote -> Proposal -> Text -> Maybe PrepushLogs -> Maybe (Text, ExitCode) -> EShell ()
rejectProposalAndAbort options remote proposal reason prepushLogs err = do
    rejectProposal options remote proposal reason prepushLogs err
    abort "Rejected"

rejectProposal :: Options -> Git.Remote -> Proposal -> Text -> Maybe PrepushLogs -> Maybe (Text, ExitCode) -> EShell ()
rejectProposal options remote proposal reason prepushLogs err = do
    let origBranchName = Proposal.toBranchName proposal
        rejectedProposal = proposal { Proposal.proposalStatus = Proposal.ProposalRejected }
        rejectBranchName = Proposal.toBranchName rejectedProposal
        suffix = case err of
            Just (msg, errCode) -> " because: '" <> reason <> "' (" <> msg <> "), exit code = " <> T.pack (show errCode)
            Nothing -> ""
        msgBody = "REJECT " <> Git.fromBranchName origBranchName <> suffix
    eprint msgBody
    sendProposalEmail options proposal ("Rejecting (" <> reason <> ")") (toHtml msgBody) prepushLogs ProposalFailureEmail
    Git.fetch & ignoreError
    Git.deleteBranch (Git.RemoteBranch remote rejectBranchName) & ignoreError -- in case it exists
    Git.deleteBranch (Git.LocalBranch rejectBranchName) & ignoreError -- in case it exists
    Git.reset Git.ResetHard Git.RefHead
    _ <- Git.createLocalBranch rejectBranchName Git.RefHead
    _ <- Git.pushRemoteTracking remote rejectBranchName Git.PushForceWithoutLease
    -- We have to be on another branch before deleting stuff, so arbitrarily picking rejected branch
    Git.checkout (Git.RefBranch $ Git.LocalBranch rejectBranchName)
    Git.deleteBranch (Git.LocalBranch origBranchName) & ignoreError
    Git.deleteBranch (Git.RemoteBranch remote origBranchName)

-- Checks that the proposal is valid (e.g. the onto branch still actually exists on the remote)
verifyProposal :: Options -> Git.Remote -> Proposal -> EShell ()
verifyProposal options remote proposal = do
    let ontoBranchName = Proposal.proposalBranchOnto proposal
    remoteBranches <- Git.remoteBranches
    unless ((remote, ontoBranchName) `elem` remoteBranches)
        $ rejectProposalAndAbort options remote proposal ("Remote branch doesn't exist: " <> Git.fromBranchName ontoBranchName) Nothing Nothing
    -- TODO: Add check that base hash (for merge proposals) is in range of commits that makes sense

-- Rebases given proposal over latest known state of its target branch
-- and pushes it to the remote.
-- If the rebase fails, rejects the proposal.
updateProposal :: Options -> Git.Remote -> (Git.Branch, Proposal) -> EShell (Maybe (Git.Branch, Proposal))
updateProposal options remote (proposalBranch, proposal) = do
    verifyProposal options remote proposal
    updateProposal' options remote (proposalBranch, proposal)

updateProposal' :: Options -> Git.Remote -> (Git.Branch, Proposal) -> EShell (Maybe (Git.Branch, Proposal))
updateProposal' options remote (proposalBranch, proposal) =
    case Proposal.proposalType proposal of
        Proposal.ProposalTypeRebase{} -> return $ Just (proposalBranch, proposal) -- nothing to update
        Proposal.ProposalTypeMerge mergeType origBaseHash -> Git.withTempLocalBranch $ \tempBranchName -> do
            let proposalBranchName = Proposal.toBranchName proposal
                ontoBranchName = Proposal.proposalBranchOnto proposal
                remoteOntoBranch = Git.RemoteBranch remote ontoBranchName

            newBaseHash <- Git.refToHash $ Git.RefBranch remoteOntoBranch
            newBaseShortHash <- Git.shortenHash newBaseHash
            eprint $ "Comparing base hashes: " <> T.intercalate " " (map (T.pack . show) [newBaseShortHash, origBaseHash])
            if newBaseShortHash == origBaseHash
            then return $ Just (proposalBranch, proposal) -- nothing to do
            else do
                updatedProposal <- makeUnique proposal { Proposal.proposalType = Proposal.ProposalTypeMerge mergeType newBaseShortHash }
                let updatedProposalBranchName = Proposal.toBranchName updatedProposal
                    remoteProposalBranch = Git.RefBranch $ Git.RemoteBranch remote proposalBranchName
                    rebasePolicy = case mergeType of
                        Proposal.MergeTypeFlat -> Git.RebaseDropMerges
                        Proposal.MergeTypeKeepMerges -> Git.RebaseKeepMerges

                Git.reset Git.ResetHard remoteProposalBranch
                Git.rebase Git.Rebase { Git.rebaseBase = Git.RefHash origBaseHash
                                      , Git.rebaseOnto = Git.RefBranch remoteOntoBranch
                                      , Git.rebasePolicy = rebasePolicy }
                    `catchError` (rejectProposalAndAbort options remote proposal "Rebase failed" Nothing . Just)

                -- create updated proposal branch
                Git.deleteLocalBranch updatedProposalBranchName & ignoreError
                _ <- Git.createLocalBranch updatedProposalBranchName Git.RefHead

                createdRemoteBranch <- Git.pushRemoteTracking remote updatedProposalBranchName Git.PushNonForce
                assert (==) createdRemoteBranch (Git.RemoteBranch remote updatedProposalBranchName) Nothing

                Git.checkout (Git.RefBranch $ Git.LocalBranch tempBranchName)
                Git.deleteLocalBranch updatedProposalBranchName

                -- Concurrency issues here:

                -- If deleting the (not-updated) proposal fails, then possibly someone else already deleted the proposal,
                -- 1. Either because a user wanted to cancel this proposal,
                -- 2. Or, they were also rebasing it (like this function)
                -- 3. Or, they were rejecting it because it failed rebase (which didn't fail for us because our remote is outdated)
                -- 4. Or, the proposal was marked as 'in progress'

                -- In any case, deleting the proposal serves as a 'commit' for operations on it, so someone else beat us to
                -- it and we must undo our actions above which are just creating a new branch of the updated proposal.

                Git.deleteRemoteBranch remote proposalBranchName >> return (Just (createdRemoteBranch, updatedProposal))
                    `catchError` (\_ -> Git.deleteRemoteBranch remote updatedProposalBranchName >> return Nothing)

----------------------------------------------------------------------
withNewBranch :: Git.Remote -> Git.BranchName -> Git.PushType -> EShell a -> EShell a
withNewBranch remote b pushType act = do
    currentRef <- Git.currentRef
    Git.deleteLocalBranch b & ignoreError
    _ <- Git.createLocalBranch b Git.RefHead
    _ <- Git.pushRemoteTracking remote b pushType
    let cleanup = do
            Git.checkout currentRef
            Git.deleteBranch (Git.LocalBranch b)
            Git.deleteBranch (Git.RemoteBranch remote b)
    act `catchError` (\e -> cleanup >> throwError e)

withLocalBranch :: Git.BranchName -> EShell a -> EShell a
withLocalBranch name act = do
    let branch = Git.LocalBranch name
    currentRef <- Git.currentRef
    Git.deleteBranch branch & ignoreError
    shouldCreate <- notElem name <$> Git.localBranches
    when shouldCreate $ void $ Git.createLocalBranch name Git.RefHead
    Git.checkout (Git.RefBranch branch)
    let cleanup = do
            Git.checkout currentRef
            when shouldCreate $ Git.deleteBranch branch
    act `catchError` (\e -> cleanup >> throwError e)

tryTakeJob :: Proposal.ServerId -> Options -> Git.Remote -> (Git.Branch, Proposal) -> EShell (Maybe Job)
tryTakeJob serverId options remote (proposalBranch, proposal) = do
    mUpdatedProposal <- Sling.updateProposal options remote (proposalBranch, proposal)
    case mUpdatedProposal of
        Nothing -> do
            -- The proposal was deleted while we were working on it. Forget about it.
            eprint "Other slave took the job or proposal deleted? Dropping"
            return Nothing
        Just (updatedProposalBranch, updatedProposal) ->
            tryTakeJob' serverId options remote updatedProposalBranch updatedProposal

prepareMergeProposal :: Git.Remote -> Git.Branch -> Proposal -> Hash -> Git.Branch -> EShell ()
prepareMergeProposal remote proposalBranch proposal baseHash niceBranch = do
    let ontoBranchName = Proposal.proposalBranchOnto proposal
    -- note: 'nice' branch and 'onto' branch may be the same
    -- branch. (e.g. proposal called 'master' with onto=master)
    remoteOntoBranchHash <- Git.refToHash (Git.RefBranch $ Git.RemoteBranch remote ontoBranchName)
    -- check that the proposal's base commit is exactly onto (should have been rebased by now):
    fullBaseHash <- Git.unshortenHash baseHash
    assert (==) fullBaseHash remoteOntoBranchHash
        . Just
        $ "Expected branch to be rebased already, but it isn't: "
        <> T.intercalate " " (map (T.pack . show) [ remoteOntoBranchHash, fullBaseHash ])
    -- point the working ('nice') branch to the proposal's head
    Git.reset Git.ResetHard (Git.RefBranch proposalBranch)
    isMerge <- Git.isMergeCommit Git.RefHead
    commits <- Git.log (Git.RefHash fullBaseHash) Git.RefHead
    if length commits == 0
    then eprint "Empty proposal, nothing to do here"
    else do
        let mergeFF =
                if isMerge || (length commits == 1)
                then Git.MergeFFOnly
                else Git.MergeNoFF
        -- go back to 'onto', decide whether to create a merge commit on
        -- top (if we should merge ff only)
        Git.checkout (Git.RefBranch $ Git.LocalBranch ontoBranchName)
        Git.reset Git.ResetHard (Git.RefBranch $ Git.RemoteBranch remote ontoBranchName)
        Git.merge mergeFF niceBranch
        when (mergeFF == Git.MergeNoFF) $
            Git.commitAmend (Proposal.proposalEmail proposal) Git.RefHead
        newHead <- Git.currentRef
        -- Fast-forward the work branch to match the merged 'onto' we do
        -- this so that the prepush script will see itself running on a
        -- branch with the name the user gave to this proposal, and not
        -- the onto branch's name.
        Git.checkout (Git.RefBranch niceBranch)
        Git.merge Git.MergeFFOnly (Git.LocalBranch ontoBranchName)
        headAfterFF <- Git.currentRef
        assert (==) newHead headAfterFF $ Just "Expected to arrive at same commit"

tryTakeJob' :: Proposal.ServerId -> Options -> Git.Remote -> Git.Branch -> Proposal -> EShell (Maybe Job)
tryTakeJob' serverId options remote proposalBranch proposal = do
    let ontoBranchName = Proposal.proposalBranchOnto proposal
        remoteOnto = Git.RefBranch $ Git.RemoteBranch remote ontoBranchName
        (baseRef, headRef) =
            case Proposal.proposalType proposal of
              Proposal.ProposalTypeMerge _mergeType base -> (Git.RefHash base, Git.RefBranch proposalBranch)
              Proposal.ProposalTypeRebase name -> (remoteOnto, Git.RefBranch $ Git.RemoteBranch remote name)

    commits <- Git.log baseRef headRef -- must be done after we verify the remote branch exists

    remoteOntoBranchHash <- Git.refToHash (Git.RefBranch $ Git.RemoteBranch remote ontoBranchName)
    let nicePrefix = if Proposal.proposalName proposal == ontoBranchName
                     then "_" -- to ensure niceBranchName never equals ontoBranchName
                     else ""
        niceBranchName = Git.mkBranchName $ nicePrefix <> Git.fromBranchName (Proposal.proposalName proposal)
        niceBranch = Git.LocalBranch niceBranchName
        finalBase = Git.RefHash remoteOntoBranchHash

    -- create local work branch, reset to proposed
    withLocalBranch niceBranchName $ do
        case Proposal.proposalType proposal of
            Proposal.ProposalTypeMerge _mergeType baseHash ->
                prepareMergeProposal remote proposalBranch proposal baseHash niceBranch

            Proposal.ProposalTypeRebase branchToRebase -> do
                Git.reset Git.ResetHard (Git.RefBranch $ Git.RemoteBranch remote branchToRebase)
                -- rebase target on onto
                Git.rebase Git.Rebase { Git.rebaseBase = remoteOnto,
                                        Git.rebaseOnto = remoteOnto,
                                        Git.rebasePolicy = Git.RebaseKeepMerges
                                      }
                    `catchError` (rejectProposalAndAbort options remote proposal "Rebase failed" Nothing . Just)
                -- rebase succeeded, we can now take this job

        finalHead <- Git.currentRef

        eprint "Switching to (new) in-progress branch"
        let forceCreateInProgress = case Proposal.proposalStatus proposal of
                Proposal.ProposalInProgress{} -> Git.PushForceWithoutLease -- can't use lease to create new branch. stupid git.
                _                             -> Git.PushNonForce

        inProgressProposal <- makeUnique proposal { Proposal.proposalStatus = Proposal.ProposalInProgress serverId }
        let inProgressBranchName = Proposal.toBranchName inProgressProposal
        eprint . T.pack $ "Creating in-progress proposal branch: " <> T.unpack (Git.fromBranchName inProgressBranchName)

        withNewBranch remote inProgressBranchName forceCreateInProgress $ do
            Git.deleteLocalBranch niceBranchName
            jobTaken <- case Proposal.proposalStatus proposal of
                Proposal.ProposalRejected -> error "ASSERTION FAILED! Shouldn't be taking rejected proposal"
                Proposal.ProposalInProgress{} | inProgressBranchName == Git.branchName proposalBranch -> return True
                _ -> do
                    eprint "Deleting proposal branch..."
                    (Git.deleteBranch proposalBranch >> return True)
                        `catchError` const (eprint "Can't delete proposal - Other slave took the job? Dropping" >> return False)
            if jobTaken
                then do
                    commitLogHtml <- formatCommitsForEmail options inProgressProposal commits <$> Git.remoteUrl remote
                    let title = if isDryRun options proposal
                                then "Running dry run"
                                else "Attempting to merge"
                    sendProposalEmail options proposal title commitLogHtml Nothing ProposalAttemptEmail
                    return . Just $ Job inProgressProposal finalBase finalHead
                else return Nothing

----------------------------------------------------------------------

transitionProposalToTarget :: Options -> Git.Remote -> Git.Ref -> Proposal -> Proposal.Prefix -> Maybe PrepushLogs -> EShell ()
transitionProposalToTarget options remote newBase proposal targetPrefix prepushLogs = do
    newBaseHash <- Git.refToHash newBase
    shortBaseHash <- Git.shortenHash newBaseHash

    let updatedProposalType = case Proposal.proposalType proposal of
            Proposal.ProposalTypeMerge mergeType _oldBase -> Proposal.ProposalTypeMerge mergeType shortBaseHash
            Proposal.ProposalTypeRebase name              -> Proposal.ProposalTypeRebase name

    updatedProposal <- makeUnique proposal { Proposal.proposalPrefix = Just targetPrefix
                                           , Proposal.proposalType = updatedProposalType
                                           , Proposal.proposalStatus = Proposal.ProposalProposed }
    let targetBranchName = Proposal.toBranchName updatedProposal
    eprint . T.pack $ "Creating target proposal branch: " <> T.unpack (Git.fromBranchName targetBranchName)
    let ontoBranchName = Proposal.proposalBranchOnto proposal
    when (targetBranchName == ontoBranchName)
        $ abort $ "Can't handle branch, onto == target: " <> Git.fromBranchName targetBranchName
    Git.deleteLocalBranch targetBranchName & ignoreError
    _ <- Git.createLocalBranch targetBranchName Git.RefHead
    _ <- Git.pushRemoteTracking remote targetBranchName Git.PushNonForce
    Git.checkout (Git.RefBranch $ Git.LocalBranch ontoBranchName)
    Git.deleteLocalBranch targetBranchName
    sendProposalEmail options proposal ("Ran successfully, moved to: " <> Proposal.prefixToText targetPrefix) "" prepushLogs ProposalSuccessEmail

transitionProposalToCompletion :: Options -> Git.Ref -> Proposal -> Maybe PrepushLogs -> EShell ()
transitionProposalToCompletion options finalHead proposal prepushLogs =
    if isDryRun options proposal
    then sendProposalEmail options proposal "Dry-run: Prepush ran successfully" "" prepushLogs ProposalSuccessEmail
    else do
        case Proposal.proposalType proposal of
            Proposal.ProposalTypeMerge _mergeType _baseRef -> do
                let ontoBranchName = Proposal.proposalBranchOnto proposal
                eprint $ "Updating: " <> Git.fromBranchName ontoBranchName
                Git.checkout (Git.RefBranch $ Git.LocalBranch ontoBranchName)
                Git.push
            Proposal.ProposalTypeRebase name  -> do
                eprint $ "Updating: " <> Git.fromBranchName name
                Git.deleteLocalBranch name & ignoreError
                Git.checkout (Git.RefBranch $ Git.LocalBranch name)
                Git.reset Git.ResetHard finalHead
                Git.pushForceWithLease

        sendProposalEmail options proposal "Merged successfully" "" prepushLogs ProposalSuccessEmail

transitionProposal :: Options -> Git.Remote -> Job -> Maybe PrepushLogs -> EShell ()
transitionProposal options remote (Job proposal finalBase finalHead) prepushLogs = do
    eprint $ "Transitioning: " <> (T.pack $ show proposal)
    case Proposal.proposalStatus proposal of
        Proposal.ProposalInProgress{} -> return ()
        _ -> abort $ "Must not be called on proposals unless they are in progress! Got: " <> (Proposal.formatProposal proposal)


    case Options.optTargetPrefix options of
        Nothing -> transitionProposalToCompletion options finalHead proposal prepushLogs
        Just targetPrefix -> transitionProposalToTarget options remote finalBase proposal targetPrefix prepushLogs

    -- Cleanup
    curHash <- Git.currentRefHash
    Git.checkout $ Git.RefHash curHash
    Git.deleteBranch (Git.LocalBranch $ Proposal.toBranchName proposal)
    Git.deleteBranch (Git.RemoteBranch remote $ Proposal.toBranchName proposal)

----------------------------------------------------------------------

runPrepush' :: PrepushLogs -> Options.PrepushCmd -> Git.Ref -> Git.Ref -> EShell ()
runPrepush' (PrepushLogs logDir logFile) (Options.PrepushCmd cmd) baseR headR = do
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

runPrepush :: Options -> Git.Remote -> Options.PrepushCmd -> PrepushLogs -> Sling.Job -> EShell ()
runPrepush options remote prepushCmd prepushLogs (Sling.Job proposal finalBase finalHead) = do
    runPrepush' prepushLogs prepushCmd finalBase finalHead
        `catchError` (rejectProposalAndAbort options remote proposal "Prepush command failed" (Just prepushLogs) . Just)
    -- TODO ensure not dirty
    eprint "Prepush command ran succesfully"
