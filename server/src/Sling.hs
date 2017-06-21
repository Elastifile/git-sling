{-# LANGUAGE OverloadedStrings #-}
module Sling
    ( Job(..)
    , tryTakeJob
    , rejectProposal
    , updateProposal
    , transitionProposal
    ) where

import           Control.Monad (unless, when, void)
import           Control.Monad.Except (MonadError (..))
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid ((<>))
import           Data.Text              (Text)
import qualified Data.Text as T
import           Text.Blaze.Html (toHtml)
import           Turtle ((&), ExitCode)

import qualified Sling.Git as Git
import           Sling.Email (sendProposalEmail, formatCommitsForEmail, EmailType(..))
import           Sling.Lib (EShell, Hash, ignoreError, assert, eprint, abort)
import           Sling.Options (Options, isDryRun)
import qualified Sling.Options as Options
import           Sling.Prepush (PrepushLogs(..))
import qualified Sling.Proposal as Proposal
import           Sling.Proposal (Proposal)



data Job
    = Job
    { jobProposal :: Proposal
    , jobInProgressBranchName :: Git.BranchName
    , jobBase :: Git.Ref
    , jobHead :: Git.Ref
    } deriving (Show)


rejectProposal :: Options -> Git.Remote -> Git.BranchName -> Proposal -> Text -> Maybe PrepushLogs -> Maybe (Text, ExitCode) -> EShell ()
rejectProposal options remote origBranchName proposal reason prepushLogs err = do
    let rejectedProposal = proposal { Proposal.proposalStatus = Proposal.ProposalRejected }
        rejectBranchName = Proposal.toBranchName rejectedProposal
        suffix = case err of
            Just (msg, errCode) -> " because: '" <> reason <> "' (" <> msg <> "), exit code = " <> T.pack (show errCode)
            Nothing -> ""
        msgBody = "REJECT " <> Git.fromBranchName origBranchName <> suffix
    eprint $ msgBody
    sendProposalEmail options proposal ("Rejecting (" <> reason <> ")") (toHtml msgBody) prepushLogs ProposalFailureEmail
    Git.fetch & ignoreError
    Git.deleteBranch (Git.RemoteBranch remote rejectBranchName) & ignoreError -- in case it exists
    Git.deleteBranch (Git.LocalBranch rejectBranchName) & ignoreError -- in case it exists
    Git.reset Git.ResetHard Git.RefHead
    _ <- Git.createLocalBranch rejectBranchName Git.RefHead
    _ <- Git.pushRemoteTracking remote rejectBranchName Git.PushForceWithoutLease
    -- We have to be on another branch before deleting stuff, so arbitrarily picking rejected branch
    Git.checkout (Git.RefBranch $ Git.LocalBranch rejectBranchName)
    Git.deleteBranch (Git.LocalBranch $ Proposal.toBranchName proposal) & ignoreError
    Git.deleteBranch (Git.RemoteBranch remote origBranchName)
    abort "Rejected"

-- Checks that the proposal is valid (e.g. the onto branch still actually exists on the remote)
verifyProposal :: Options -> Git.Remote -> Proposal -> EShell ()
verifyProposal options remote proposal = do
    let ontoBranchName = Proposal.proposalBranchOnto proposal
        proposalBranchName = Proposal.toBranchName proposal
    remoteBranches <- Git.remoteBranches
    unless ((remote, ontoBranchName) `elem` remoteBranches)
        $ Sling.rejectProposal options remote proposalBranchName proposal ("Remote branch doesn't exist: " <> Git.fromBranchName ontoBranchName) Nothing Nothing
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
                remoteOntoBranch = Git.RemoteBranch remote $ ontoBranchName

            newBaseHash <- Git.refToHash $ Git.RefBranch remoteOntoBranch
            newBaseShortHash <- Git.shortenHash newBaseHash
            let updatedProposal = proposal { Proposal.proposalType = Proposal.ProposalTypeMerge mergeType newBaseShortHash }
                updatedProposalBranchName = Proposal.toBranchName updatedProposal

            if (updatedProposalBranchName == proposalBranchName)
            then return $ Just (proposalBranch, proposal) -- nothing to do
            else do
                let remoteProposalBranch = Git.RefBranch $ Git.RemoteBranch remote $ proposalBranchName
                    rebasePolicy = case mergeType of
                        Proposal.MergeTypeFlat -> Git.RebaseDropMerges
                        Proposal.MergeTypeKeepMerges -> Git.RebaseKeepMerges

                Git.reset Git.ResetHard remoteProposalBranch
                Git.rebase Git.Rebase { Git.rebaseBase = Git.RefHash origBaseHash
                                      , Git.rebaseOnto = Git.RefBranch $ remoteOntoBranch
                                      , Git.rebasePolicy = rebasePolicy }
                    `catchError` (\e -> rejectProposal options remote proposalBranchName proposal "Rebase failed" Nothing (Just e))

                -- create updated proposal branch
                Git.deleteLocalBranch updatedProposalBranchName & ignoreError
                _ <- Git.createLocalBranch updatedProposalBranchName Git.RefHead

                createdRemoteBranch <- Git.pushRemoteTracking remote updatedProposalBranchName Git.PushForceWithLease
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
    res <- act `catchError` (\e -> cleanup >> throwError e)
    return res

withLocalBranch :: Git.BranchName -> EShell a -> EShell a
withLocalBranch name act = do
    currentRef <- Git.currentRef
    Git.deleteBranch branch & ignoreError
    Git.localBranches >>= (liftIO . mapM_ print)
    shouldCreate <- not . elem name <$> Git.localBranches
    when shouldCreate $ void $ Git.createLocalBranch name Git.RefHead
    Git.checkout (Git.RefBranch branch)
    let cleanup = do
            Git.checkout currentRef
            when shouldCreate $ Git.deleteBranch branch
    res <- act `catchError` (\e -> cleanup >> throwError e)
    return res
    where branch = Git.LocalBranch name


tryTakeJob :: Proposal.ServerId -> Options -> Git.Remote -> (Git.Branch, Proposal) -> EShell (Maybe Job)
tryTakeJob serverId options remote (proposalBranch, proposal) = do
    mUpdatedProposal <- Sling.updateProposal options remote (proposalBranch, proposal)
    case mUpdatedProposal of
        Nothing -> do
            -- The proposal was deleted while we were working on it. Forget about it.
            eprint "Other slave took the job or proposal deleted? Dropping"
            return Nothing
        Just (updatedProposalBranch, updatedProposal) -> do
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
        niceBranchName = Git.mkBranchName $ nicePrefix <> (Git.fromBranchName $ Proposal.proposalName proposal)
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
                    `catchError` (Sling.rejectProposal options remote (Git.branchName proposalBranch) proposal "Rebase failed" Nothing . Just)
                -- rebase succeeded, we can now take this job

        finalHead <- Git.currentRef

        eprint "Switching to (new) in-progress branch"
        let forceCreateInProgress = case Proposal.proposalStatus proposal of
                Proposal.ProposalInProgress{} -> Git.PushForceWithoutLease -- can't use lease to create new branch. stupid git.
                _                             -> Git.PushNonForce

            inProgressBranchName = Proposal.toBranchName $ proposal { Proposal.proposalStatus = Proposal.ProposalInProgress serverId }
        eprint . T.pack $ "Creating in-progress proposal branch: " <> T.unpack (Git.fromBranchName inProgressBranchName)

        withNewBranch remote inProgressBranchName forceCreateInProgress $ do
            Git.deleteLocalBranch niceBranchName
            jobTaken <- case Proposal.proposalStatus proposal of
                Proposal.ProposalRejected -> error "ASSERTION FAILED! Shouldn't be taking rejected proposal"
                Proposal.ProposalInProgress{} | inProgressBranchName == (Git.branchName proposalBranch) -> return True
                _ -> do
                    eprint "Deleting proposal branch..."
                    (Git.deleteBranch proposalBranch >> return True)
                        `catchError` (const $ eprint "Can't delete proposal - Other slave took the job? Dropping" >> return False)
            case jobTaken of
                True -> do
                    commitLogHtml <- formatCommitsForEmail options proposal commits <$> Git.remoteUrl remote
                    let title = if isDryRun options proposal
                                then "Running dry run"
                                else "Attempting to merge"
                    sendProposalEmail options proposal title commitLogHtml Nothing ProposalAttemptEmail
                    return . Just $ Job proposal inProgressBranchName finalBase finalHead
                False -> return Nothing

----------------------------------------------------------------------

transitionProposalToTarget :: Options -> Git.Remote -> Git.Ref -> Proposal -> Proposal.Prefix -> PrepushLogs -> EShell ()
transitionProposalToTarget options remote newBase proposal targetPrefix prepushLogs = do
    newBaseHash <- Git.refToHash newBase
    shortBaseHash <- Git.shortenHash newBaseHash
    let updatedProposalType = case Proposal.proposalType proposal of
            Proposal.ProposalTypeMerge mergeType _oldBase -> Proposal.ProposalTypeMerge mergeType shortBaseHash
            Proposal.ProposalTypeRebase name              -> Proposal.ProposalTypeRebase name

        targetBranchName = Proposal.toBranchName $ proposal { Proposal.proposalPrefix = Just targetPrefix
                                                            , Proposal.proposalType = updatedProposalType
                                                            , Proposal.proposalStatus = Proposal.ProposalProposed }
    eprint . T.pack $ "Creating target proposal branch: " <> T.unpack (Git.fromBranchName targetBranchName)
    let ontoBranchName = Proposal.proposalBranchOnto proposal
    when (targetBranchName == ontoBranchName)
        $ abort $ "Can't handle branch, onto == target: " <> (Git.fromBranchName targetBranchName)
    Git.deleteLocalBranch targetBranchName & ignoreError
    _ <- Git.createLocalBranch targetBranchName Git.RefHead
    _ <- Git.pushRemoteTracking remote targetBranchName Git.PushNonForce
    Git.checkout (Git.RefBranch $ Git.LocalBranch ontoBranchName)
    Git.deleteLocalBranch targetBranchName
    sendProposalEmail options proposal ("Ran successfully, moved to: " <> Proposal.prefixToText targetPrefix) "" (Just prepushLogs) ProposalSuccessEmail

transitionProposalToCompletion :: Options -> Git.Ref -> Proposal -> PrepushLogs -> EShell ()
transitionProposalToCompletion options finalHead proposal prepushLogs = do
    if isDryRun options proposal
    then sendProposalEmail options proposal "Dry-run: Prepush ran successfully" "" (Just prepushLogs) ProposalSuccessEmail
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

        sendProposalEmail options proposal "Merged successfully" "" (Just prepushLogs) ProposalSuccessEmail

transitionProposal :: Options -> Git.Remote -> Git.Ref -> Git.Ref -> Proposal -> PrepushLogs -> EShell ()
transitionProposal options remote finalBase finalHead proposal prepushLogs =
    case Options.optTargetPrefix options of
        Nothing -> transitionProposalToCompletion options finalHead proposal prepushLogs
        Just targetPrefix -> transitionProposalToTarget options remote finalBase proposal targetPrefix prepushLogs
