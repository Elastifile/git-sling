{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad          (forM_, when)
import           Control.Monad.Error    (MonadError (..))
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Sling.Git              (Branch (..), BranchName, Ref (..),
                                         Remote (..), branchFullName,
                                         branchName, fromBranchName,
                                         mkBranchName)
import qualified Sling.Git              as Git
import           Sling.Lib              (EShell, NonEmptyText, abort, eview,
                                         fromNonEmptyText, ignoreError,
                                         nonEmptyText, notSpace, runEShell,
                                         singleMatch)
import           Sling.Lib              (eprocsL)
import           Sling.Proposal
import           Turtle                 (ExitCode, (&))

import qualified Data.List              as List
import           Data.Monoid            (mempty, (<>))


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
    output <- eprocsL "bash" ["./tools/prepush.sh", Git.refName baseR, Git.refName headR]
    -- TODO log it
    return ()

-- staging :: BranchName
-- staging = "staging"

-- master :: BranchName
-- master = "master"

origin :: Remote
origin = Remote "origin"

resetLocalOnto :: Proposal -> EShell ()
resetLocalOnto proposal = do
    let ontoBranchName = proposalBranchOnto proposal
    Git.checkout (LocalBranch ontoBranchName)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin ontoBranchName)

abortAttempt :: Proposal -> ExitCode -> EShell ()
abortAttempt proposal err = do
    Git.rebaseAbort & ignoreError
    Git.reset Git.ResetHard RefHead
    resetLocalOnto proposal
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

attemptBranchOrAbort :: Branch -> Proposal -> EShell ()
attemptBranchOrAbort branch proposal =
    (attemptBranch branch proposal) `catchError` (abortAttempt proposal)

attemptBranch :: Branch -> Proposal -> EShell ()
attemptBranch branch proposal = do
    -- cleanup leftover state from previous runs
    Git.rebaseAbort & ignoreError
    Git.reset Git.ResetHard RefHead
    resetLocalOnto proposal

    liftIO $ putStrLn . T.unpack $ "Attempting proposal: " <> formatProposal proposal

    let name = proposalName proposal
        pBranchName = mkBranchName $ fromNonEmptyText name
        localProposalBranch = LocalBranch $ pBranchName
        ontoBranchName = proposalBranchOnto proposal
        remoteOnto = RefBranch $ RemoteBranch origin ontoBranchName

    Git.deleteBranch localProposalBranch & ignoreError
    _ <- Git.createLocalBranch pBranchName RefHead
    Git.reset Git.ResetHard (RefBranch branch)

    liftIO $ putStrLn "Commits: "
    commits <- Git.log (proposalBranchBase proposal) (RefBranch branch)
    liftIO $ mapM_ print commits

    -- Rebase over staging
    Git.rebase (proposalBranchBase proposal) remoteOnto
        `catchError` (rejectProposal proposal "Rebase failed")

    liftIO $ putStrLn "Commits (after rebase): "
    commitsAfter <- Git.log (proposalBranchBase proposal) (RefBranch branch)
    liftIO $ mapM_ print commitsAfter

    rebasedHead <- Git.currentRef

    Git.checkout (LocalBranch ontoBranchName)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin ontoBranchName)
    finalBase <- Git.currentRef
    isMerge <- Git.isMergeCommit rebasedHead
    let mergeFF =
            if isMerge || (length commits == 1)
            then Git.MergeFFOnly
            else Git.MergeNoFF
    Git.merge mergeFF localProposalBranch
    when (mergeFF == Git.MergeNoFF) $
        Git.commitAmend (proposalEmail proposal) Git.RefHead

    finalHead <- Git.currentRef

    -- DO IT!
    runPrepush finalBase finalHead
        `catchError` (rejectProposal proposal "Prepush command failed")

    liftIO $ putStrLn . T.unpack $ "Updating: " <> (fromBranchName ontoBranchName)
    Git.checkout (LocalBranch ontoBranchName) -- in case script moved git
    -- TODO ensure not dirty
    Git.push -- TODO -u origin master

    liftIO $ putStrLn "Deleting proposal branch..."
    Git.deleteLocalBranch pBranchName
    Git.deleteRemoteBranch origin pBranchName & ignoreError
    Git.deleteBranch branch

    liftIO $ putStrLn . T.unpack $ "Finished handling proposal " <> formatProposal proposal

main :: IO ()
main = runEShell $ do
    liftIO $ putStr "Fetching..."
    Git.fetch
    liftIO $ putStrLn "Done."
    remoteBranches <- Git.remoteBranches
    let verifyRemoteBranch rb =
            when (not $ elem rb remoteBranches)
            $ abort $ "No remote branch: " <> T.pack (show rb)

    let proposedBranches =
            List.sort
            $ filter (\b -> proposedPrefix `T.isPrefixOf` (fromBranchName $ remoteBranchName b))
            $ remoteBranches

    forM_ (catMaybes
           $ map (\branch -> (branch,) <$> parseProposal (branchName branch)) proposedBranches)
           (uncurry attemptBranchOrAbort)



