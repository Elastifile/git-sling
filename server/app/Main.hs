{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Monad (forM_, when)
import           Control.Monad.Error (MonadError (..))
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Sling.Git              (Branch (..), BranchName, Ref (..),
                                         Remote (..), branchFullName,
                                         branchName, fromBranchName,
                                         mkBranchName)
import qualified Sling.Git as Git
import           Sling.Lib              (EShell, NonEmptyText, abort, eview,
                                         fromNonEmptyText, ignoreError, eprocsIn,
                                         nonEmptyText, notSpace, runEShell,
                                         singleMatch)
import           Sling.Lib (eprocsL, formatEmail)
import           Sling.Proposal
import           Turtle (ExitCode, (&))

import qualified Data.List as List
import           Data.Monoid (mempty, (<>))
import           System.Environment (getArgs)
import           System.IO.Temp (createTempDirectory)
import           Network.Mail.Mime (simpleMail, Address(..), renderMail', sendmailCustomCaptureOutput)

runPrepush :: FilePath -> [String] -> Ref -> Ref -> EShell ()
runPrepush logFile cmd baseR headR = do
    let args = T.intercalate " " $ (map T.pack cmd) ++ [Git.refName baseR, Git.refName headR]
    liftIO $ putStrLn . T.unpack $ "Executing bash with: " <> args <> " output goes to: " <> (T.pack logFile)
    output <- eprocsL "bash" ["-c", args <> " &>" <> (T.pack logFile)]
    -- TODO delete log if successful?
    return ()

origin :: Remote
origin = Remote "origin"

resetLocalOnto :: Proposal -> EShell ()
resetLocalOnto proposal = do
    let ontoBranchName = proposalBranchOnto proposal
    Git.checkout (LocalBranch ontoBranchName)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin ontoBranchName)

sendProposalEmail :: Proposal -> Text -> Text -> Maybe FilePath -> EShell ()
sendProposalEmail proposal subject body logFile = do
    let attachments =
            case logFile of
                Nothing -> []
                Just f -> [("text/plain", f)]
    mail <- liftIO $ simpleMail
        (Address Nothing $ formatEmail $ proposalEmail proposal)
        (Address Nothing $ "elasti-prepush@elastifile.com")
        subject
        (L.fromStrict body)
        ""
        attachments

    renderdBS <- (liftIO $ renderMail' mail)
    let msmtp_conf_file = "/opt/msmtp.conf"
    eprocsIn "msmtp" ["-C", msmtp_conf_file, formatEmail $ proposalEmail proposal] $ return (L.toStrict $ decodeUtf8 renderdBS)
    return ()


abortAttempt :: Proposal -> ExitCode -> EShell ()
abortAttempt proposal err = do
    liftIO $ putStrLn "ABORTING..."
    sendProposalEmail proposal "Aborting" "" Nothing
    Git.rebaseAbort & ignoreError
    Git.reset Git.ResetHard RefHead
    resetLocalOnto proposal
    abort "Aborted"


rejectProposal :: Proposal -> Text -> Maybe FilePath -> ExitCode -> EShell ()
rejectProposal proposal msg logFile err = do
    let rejectedProposal = proposal { proposalStatus = ProposalRejected }
        rejectBranchName = formatProposal rejectedProposal
        origBranchName = formatProposal proposal
        msgBody = "REJECT " <> origBranchName <> " because: '" <> msg <> "', exit code = " <> (T.pack $ show err)
    liftIO $ putStrLn . T.unpack $ msgBody
    sendProposalEmail proposal ("Rejecting (" <> msg <> ")") msgBody logFile
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

attemptBranchOrAbort :: [String] -> Branch -> Proposal -> EShell ()
attemptBranchOrAbort cmd branch proposal = do
    dirPath <- liftIO $ createTempDirectory "/tmp" "sling.log"
    (attemptBranch dirPath cmd branch proposal) `catchError` (abortAttempt proposal)

attemptBranch :: FilePath -> [String] -> Branch -> Proposal -> EShell ()
attemptBranch logDir cmd branch proposal = do
    -- cleanup leftover state from previous runs
    Git.rebaseAbort & ignoreError
    Git.reset Git.ResetHard RefHead
    Git.fetch
    resetLocalOnto proposal

    liftIO $ putStrLn . T.unpack $ "Attempting proposal: " <> formatProposal proposal

    let niceBranchName = mkBranchName $ slingPrefix <> "/work/" <> fromBranchName (proposalName proposal)
        niceBranch = LocalBranch $ niceBranchName
        ontoBranchName = proposalBranchOnto proposal
        remoteOnto = RefBranch $ RemoteBranch origin ontoBranchName

    Git.deleteBranch niceBranch & ignoreError
    (Git.createLocalBranch niceBranchName RefHead >> pure ()) & ignoreError
    Git.checkout niceBranch
    Git.reset Git.ResetHard (RefBranch branch)

    liftIO $ putStrLn "Commits: "
    commits <- Git.log (proposalBranchBase proposal) (RefBranch branch)
    liftIO $ mapM_ print commits

    -- Rebase onto target
    Git.rebase (proposalBranchBase proposal) remoteOnto
        `catchError` (rejectProposal proposal "Rebase failed" Nothing)

    liftIO $ putStrLn "Commits (after rebase): "
    commitsAfter <- Git.log (proposalBranchBase proposal) (RefBranch branch)
    liftIO $ mapM_ print commitsAfter

    Git.checkout (LocalBranch ontoBranchName)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin ontoBranchName)
    finalBase <- Git.currentRef
    isMerge <- Git.isMergeCommit (RefBranch branch)
    let mergeFF =
            if isMerge || (length commits == 1)
            then Git.MergeFFOnly
            else Git.MergeNoFF
    Git.merge mergeFF niceBranch
    when (mergeFF == Git.MergeNoFF) $
        Git.commitAmend (proposalEmail proposal) Git.RefHead

    finalHead <- Git.currentRef

    -- DO IT!
    logFileName <- T.unpack . head <$> eprocsL "mktemp" ["-p", T.pack logDir, "prepush.XXXXXXX.log"]
    runPrepush logFileName cmd finalBase finalHead
        `catchError` (rejectProposal proposal "Prepush command failed" (Just logFileName))

    liftIO $ putStrLn . T.unpack $ "Updating: " <> (fromBranchName ontoBranchName)
    Git.checkout (LocalBranch ontoBranchName) -- in case script moved git
    -- TODO ensure not dirty
    Git.push -- TODO -u origin master

    sendProposalEmail proposal "Merged successfully" "" (Just logFileName)

    -- TODO delete logfile name

    liftIO $ putStrLn "Deleting proposal branch..."
    Git.deleteLocalBranch niceBranchName
    Git.deleteRemoteBranch origin niceBranchName & ignoreError
    Git.deleteBranch branch

    liftIO $ putStrLn . T.unpack $ "Finished handling proposal " <> formatProposal proposal

usage :: String
usage = concat $ List.intersperse "\n" $
    [ "Usage: sling-server COMMAND"
    , ""
    , "where COMMAND is the prepush command to run on each attempted branch."
    ]

main :: IO ()
main = runEShell $ do
    prepushCmd <- liftIO getArgs
    when (null prepushCmd) $ do
        liftIO $ putStrLn usage
        abort "No prepush command given!"
    Git.fetch
    remoteBranches <- Git.remoteBranches
    let verifyRemoteBranch rb =
            when (not $ elem rb remoteBranches)
            $ abort $ "No remote branch: " <> T.pack (show rb)

    let proposedBranches =
            List.sort
            $ filter (\b -> proposedPrefix `T.isPrefixOf` (fromBranchName $ snd b))
            $ remoteBranches

    liftIO $ mapM print proposedBranches
    forM_ (catMaybes
           $ map (\branch -> (branch,) <$> parseProposal (branchName branch)) (map (uncurry Git.RemoteBranch) proposedBranches))
           (uncurry $ attemptBranchOrAbort prepushCmd)



