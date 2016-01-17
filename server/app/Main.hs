{-# LANGUAGE OverloadedStrings #-}
-- #!/usr/bin/env stack
-- -- stack --verbosity silent --resolver lts-3.22 --install-ghc runghc --package turtle
module Main where

import           Control.Monad (when, forM_)
import           Control.Monad.Error (MonadError(..))
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T

import           Turtle ((&), ExitCode)
import           Sling.Lib (eview, abort, runEShell, EShell, notSpace, singleMatch, ignoreError, NonEmptyText, nonEmptyText, fromNonEmptyText)
import           Sling.Git (Remote(..), Branch(..), Ref(..), branchFullName, branchName, BranchName, mkBranchName, fromBranchName)
import qualified Sling.Git as Git
import           Sling.Proposal
import           Sling.Lib (eprocsL)

import qualified Data.List as List
import           Data.Monoid ((<>), mempty)


-- git remote prune origin
-- git fetch
-- git checkout staging
-- git reset --hard origin/staging
-- git merge --ff-only origin/master
-- git push

-- git branch -r | \
--     sed -e 's,^ *origin/\(.*\),\1,g' | \
--     grep "^$SOURCE_BRANCH_PREFIX.*" | \
--     sort -g -t '/' | \
--     xargs -r -n1  -t $SOURCE_DIR/attempt-branch.sh $SOURCE_BRANCH_PREFIX "$COMMAND"


runPrepush :: Ref -> Ref -> EShell ()
runPrepush baseR headR = do
    output <- eprocsL "./tools/prepush.sh" [Git.refName baseR, Git.refName headR]
    -- TODO log it
    return ()

staging :: BranchName
staging = "staging"

master :: BranchName
master = "master"

origin :: Remote
origin = Remote "origin"

abortAttempt :: ExitCode -> EShell ()
abortAttempt err = do
    Git.rebaseAbort & ignoreError
    Git.reset Git.ResetHard RefHead
    Git.checkout (LocalBranch staging)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin master)
    abort "Aborted"


rejectProposal :: Proposal -> Text -> ExitCode -> EShell ()
rejectProposal proposal msg err = do
    let rejectedProposal = proposal { proposalStatus = ProposalRejected }
        rejectBranchName = formatProposal rejectedProposal
        origBranchName = formatProposal proposal
    liftIO $ putStrLn . T.unpack $ "REJECT " <> origBranchName <> " because: " <> msg <> " failed, exit code = " <> (T.pack $ show err)
    Git.fetch & ignoreError
    Git.deleteBranch (RemoteBranch origin $ mkBranchName rejectBranchName) & ignoreError -- in case it exists
    Git.deleteBranch (LocalBranch $ mkBranchName rejectBranchName) & ignoreError -- in case it exists
    _ <- Git.createLocalBranch (mkBranchName rejectBranchName) RefHead
    _ <- Git.createRemoteTrackingBranch origin $ mkBranchName rejectBranchName
    Git.deleteBranch (LocalBranch . mkBranchName $ formatProposal proposal) & ignoreError
    Git.deleteBranch (RemoteBranch origin $ mkBranchName origBranchName)
    abort "Rejected"

attemptBranch :: Proposal -> EShell ()
attemptBranch proposal = do
    -- cleanup leftover state from previous runs
    Git.rebaseAbort & ignoreError
    Git.reset Git.ResetHard RefHead

    liftIO $ putStrLn . T.unpack $ "Attempting proposal: " <> formatProposal proposal

    fastForwardStaging

    let name = proposalName proposal
        pBranchName = mkBranchName $ fromNonEmptyText name
        localProposalBranch = LocalBranch $ pBranchName
        remoteStaging = RefBranch $ RemoteBranch origin staging

    Git.deleteBranch localProposalBranch & ignoreError
    _ <- Git.createLocalBranch pBranchName RefHead
    Git.reset Git.ResetHard (proposalBranchHead proposal)

    liftIO $ putStrLn "Commits: "
    commits <- Git.log (proposalBranchBase proposal) (proposalBranchHead proposal)
    liftIO $ mapM_ print commits

    -- Rebase over staging
    Git.rebase (proposalBranchBase proposal) remoteStaging
        `catchError` (rejectProposal proposal "Rebase failed")

    rebasedHead <- Git.currentRef

    Git.checkout (LocalBranch staging)
    finalBase <- Git.currentRef
    isMerge <- Git.isMergeCommit rebasedHead
    let mergeFF =
            if isMerge || (length commits == 1)
            then Git.MergeFFOnly
            else Git.MergeNoFF
    Git.merge mergeFF localProposalBranch
    Git.commitAmend (proposalEmail proposal) Git.RefHead

    finalHead <- Git.currentRef

    -- DO IT!
    runPrepush finalBase finalHead
        `catchError` (rejectProposal proposal "Prepush command failed")

    liftIO $ putStrLn "Updating master..."
    Git.fetch
    Git.checkout (LocalBranch master)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin master)
    Git.merge Git.MergeFFOnly (LocalBranch staging)
    Git.push -- TODO -u origin master

    liftIO $ putStrLn "Updating staging..."
    Git.checkout (LocalBranch staging)
    Git.merge Git.MergeFFOnly (LocalBranch master)
    Git.push

    liftIO $ putStrLn "Deleting proposal branch..."
    Git.deleteLocalBranch pBranchName
    Git.deleteRemoteBranch origin pBranchName

    liftIO $ putStrLn . T.unpack $ "Finished handling proposal " <> formatProposal proposal

fastForwardStaging :: EShell ()
fastForwardStaging = do
    Git.checkout (LocalBranch master)
    Git.reset Git.ResetHard (RefBranch (RemoteBranch origin master))
    Git.checkout (LocalBranch staging)
    Git.reset Git.ResetHard (RefBranch (RemoteBranch origin staging))
    Git.merge Git.MergeFFOnly (RemoteBranch origin master)
    Git.push

main :: IO ()
main = runEShell $ do
    -- liftIO $ putStr "Fetching..."
    -- gitFetch
    -- liftIO $ putStrLn "Done."
    eview Git.status
    remoteBranches <- Git.remoteBranches
    let remoteStaging = (RemoteBranch origin staging)
        verifyRemoteBranch rb =
            when (not $ elem rb remoteBranches)
            $ abort $ "No remote branch: " <> T.pack (show rb)

    verifyRemoteBranch remoteStaging

    forM_ remoteBranches (liftIO . putStrLn . T.unpack . branchFullName)

    let proposedBranches =
            List.sort
            $ filter (\b -> proposedPrefix `T.isPrefixOf` (fromBranchName $ remoteBranchName b))
            $ remoteBranches

    forM_ (catMaybes $ map (parseProposal . branchName) proposedBranches) (\x -> attemptBranch x `catchError` abortAttempt)



