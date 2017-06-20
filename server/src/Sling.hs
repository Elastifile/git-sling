{-# LANGUAGE OverloadedStrings #-}
module Sling where

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

-- Rebases given proposal over latest known state of its target branch
-- and pushes it to the remote.
-- If the rebase fails, rejects the proposal.
rebaseProposal :: Options -> Git.Remote -> Proposal -> EShell ()
rebaseProposal options remote proposal =
    case Proposal.proposalType proposal of
        Proposal.ProposalTypeRebase{} -> return ()
        Proposal.ProposalTypeMerge mergeType baseHash -> Git.withTempLocalBranch $ \_tempBranchName -> do
            let proposalBranchName = Proposal.toBranchName proposal
                remoteProposalBranch = Git.RefBranch $ Git.RemoteBranch remote $ proposalBranchName
                remoteOntoBranch = Git.RefBranch $ Git.RemoteBranch remote $ Proposal.proposalBranchOnto proposal
                rebasePolicy = case mergeType of
                    Proposal.MergeTypeFlat -> Git.RebaseDropMerges
                    Proposal.MergeTypeKeepMerges -> Git.RebaseKeepMerges
            Git.deleteLocalBranch proposalBranchName & ignoreError
            _ <- Git.createLocalBranch proposalBranchName Git.RefHead
            Git.reset Git.ResetHard remoteProposalBranch
            Git.rebase Git.Rebase { Git.rebaseBase = Git.RefHash baseHash
                                  , Git.rebaseOnto = remoteOntoBranch
                                  , Git.rebasePolicy = rebasePolicy }
                `catchError` (\e -> rejectProposal options remote proposalBranchName proposal "Rebase failed" Nothing (Just e))
            createdRemoteBranch <- Git.pushRemoteTracking remote proposalBranchName Git.PushForceWithLease
            assert (==) (Git.RefBranch createdRemoteBranch) remoteProposalBranch Nothing

