{-# LANGUAGE OverloadedStrings #-}
module Sling where

import           Control.Monad (unless)
import           Control.Monad.Except (MonadError (..))
import           Data.Monoid ((<>))
import           Data.Text              (Text)
import qualified Data.Text as T
import           Text.Blaze.Html (toHtml)
import           Turtle ((&), ExitCode)

import qualified Sling.Git as Git
import           Sling.Email (sendProposalEmail, EmailType(..))
import           Sling.Lib (EShell, ignoreError, assert, eprint, abort)
import           Sling.Options (Options)
import           Sling.Prepush (PrepushLogs(..))
import qualified Sling.Proposal as Proposal
import           Sling.Proposal (Proposal)


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

