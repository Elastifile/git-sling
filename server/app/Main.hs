{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Monad (forM_, when, unless, join)
import           Control.Monad.Except (MonadError (..))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (mapMaybe)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Sling.Git                     (Branch (..), Ref (..),
                                                Remote (..), branchName,
                                                fromBranchName, mkBranchName)
import qualified Sling.Git as Git
import           Sling.Lib                     (EShell, Email (..), abort,
                                                eprocsIn, eprocsL, formatEmail,
                                                ignoreError, runEShell, fromHash)
import           Sling.Proposal
import           Turtle (ExitCode, (&))

import qualified Data.List as List
import           Data.Monoid ((<>))
import           Network.Mail.Mime (Mail)
import qualified Network.Mail.Mime as Mail
import           System.Environment (getArgs)
import           System.IO.Temp (createTempDirectory)
import           Text.Blaze.Html (toHtml, (!))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

runPrepush :: FilePath -> [String] -> Ref -> Ref -> EShell ()
runPrepush logFile cmd baseR headR = do
    let args = T.intercalate " " $ map T.pack cmd ++ [Git.refName baseR, Git.refName headR]
    liftIO $ putStrLn . T.unpack $ "Executing bash with: " <> args <> " output goes to: " <> T.pack logFile
    _output <- eprocsL "bash" ["-c", args <> " &>" <> T.pack logFile]
    -- TODO delete log if successful?
    return ()

origin :: Remote
origin = Remote "origin"

sourceEmail :: Email
sourceEmail = Email "elasti-prepush" "elastifile.com"

resetLocalOnto :: Proposal -> EShell ()
resetLocalOnto proposal = do
    let ontoBranchName = proposalBranchOnto proposal
    Git.checkout (LocalBranch ontoBranchName)
    Git.reset Git.ResetHard (RefBranch $ RemoteBranch origin ontoBranchName)

addAttachment :: Text -> FilePath -> Text -> Mail -> IO Mail
addAttachment ct fn attachedFN mail = do
    content <- LBS.readFile fn
    let part = Mail.Part ct Mail.QuotedPrintableText (Just attachedFN) []
               (encodeUtf8 ("<html><body><pre>" <> renderHtml (toHtml . L.toStrict $ decodeUtf8 content) <> "</pre></body></html>"))
    return $ Mail.addPart [part] mail

sendProposalEmail :: Proposal -> Text -> H.Html -> Maybe FilePath -> EShell ()
sendProposalEmail proposal subject body logFile = do
    mail1 <- liftIO $ Mail.simpleMail
        (Mail.Address Nothing $ formatEmail $ proposalEmail proposal)
        (Mail.Address Nothing $ formatEmail sourceEmail)
        ((if proposalDryRun proposal then "(dry run) " else "")
         <> fromBranchName (proposalName proposal)
         <> " (" <> formatProposal proposal <> ")")
        ""
        (renderHtml $ do
                H.p . H.b $ fromString $ T.unpack subject
                body)
        []

    mail <- case logFile of
        Nothing -> return mail1
        Just f -> liftIO $ addAttachment "text/html; charset=utf-8" f "log.html" mail1

    renderdBS <- liftIO $ Mail.renderMail' mail
    let msmtp_conf_file = "/opt/msmtp.conf"
    _ <- eprocsIn "msmtp" ["-C", msmtp_conf_file, formatEmail $ proposalEmail proposal] $ return (L.toStrict $ decodeUtf8 renderdBS)
    return ()


abortAttempt :: Proposal -> ExitCode -> EShell ()
abortAttempt proposal _err = do
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
        msgBody = "REJECT " <> origBranchName <> " because: '" <> msg <> "', exit code = " <> T.pack (show err)
    liftIO $ putStrLn . T.unpack $ msgBody
    sendProposalEmail proposal ("Rejecting (" <> msg <> ")") (toHtml msgBody) logFile
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
    attemptBranch dirPath cmd branch proposal `catchError` abortAttempt proposal

htmlFormatCommit :: Maybe Text -> Git.LogEntry -> H.Html
htmlFormatCommit urlPrefix l = do
    let hashCol =
            case (join $ Git.githubCommitUrl (Git.logEntryFullHash l) <$> urlPrefix) of
                Nothing -> fromString . T.unpack $ fromHash $ Git.logEntryShortHash l
                Just url -> H.a ! A.href (fromString $ T.unpack url) $ fromString . T.unpack $ fromHash $ Git.logEntryShortHash l
    H.td hashCol
    H.td $ fromString $ T.unpack $ Git.logEntryAuthor l
    H.td $ fromString $ T.unpack $ Git.logEntryTitle l

htmlFormatCommitLog :: [Git.LogEntry] -> Maybe Text -> H.Html
htmlFormatCommitLog commits urlPrefix = do
    H.p "Commits:"
    H.table . H.tbody $ mapM_ (H.tr . htmlFormatCommit urlPrefix) commits

proposalEmailHeader :: Proposal -> [Git.LogEntry] -> Maybe Text -> H.Html
proposalEmailHeader proposal commits baseUrl = do
    H.p . H.b $ "Proposal"
    H.p . fromString $ "Proposed by: " <> (T.unpack $ formatEmail . proposalEmail $ proposal)
    H.p $ do
        H.span "Onto branch: "
        H.b (fromString . T.unpack . fromBranchName . proposalBranchOnto $ proposal)
        when (proposalDryRun proposal) $ H.span "(Dry run)"
    htmlFormatCommitLog commits baseUrl

attemptBranch :: FilePath -> [String] -> Branch -> Proposal -> EShell ()
attemptBranch logDir cmd branch proposal = do
    Git.fetch

    liftIO $ putStrLn . T.unpack $ "Attempting proposal: " <> formatProposal proposal
    liftIO $ putStrLn "Commits: "
    commits <- Git.log (proposalBranchBase proposal) (RefBranch branch)
    liftIO $ mapM_ print commits

    commitLogHtml <- proposalEmailHeader proposal commits <$> Git.remoteUrl origin
    let title = if proposalDryRun proposal
                then "Running dry run"
                else "Attempting to merge"
    sendProposalEmail proposal title commitLogHtml Nothing

    -- cleanup leftover state from previous runs
    Git.rebaseAbort & ignoreError
    Git.reset Git.ResetHard RefHead
    resetLocalOnto proposal

    remoteBranches <- Git.remoteBranches

    let niceBranchName = mkBranchName $ slingPrefix <> "/work/" <> fromBranchName (proposalName proposal)
        niceBranch = LocalBranch niceBranchName
        ontoBranchName = proposalBranchOnto proposal
        remoteOnto = RefBranch $ RemoteBranch origin ontoBranchName
        verifyRemoteBranch rb =
            unless (elem rb remoteBranches)
            $ abort $ "No remote branch: " <> T.pack (show rb)

    verifyRemoteBranch (origin, ontoBranchName)

    Git.deleteBranch niceBranch & ignoreError
    (Git.createLocalBranch niceBranchName RefHead >> pure ()) & ignoreError
    Git.checkout niceBranch
    Git.reset Git.ResetHard (RefBranch branch)


    -- Rebase onto target
    Git.rebase (proposalBranchBase proposal) remoteOnto
        `catchError` rejectProposal proposal "Rebase failed" Nothing

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
    logFileName <- T.unpack . head <$> eprocsL "mktemp" ["-p", T.pack logDir, "prepush.XXXXXXX.txt"]
    runPrepush logFileName cmd finalBase finalHead
        `catchError` rejectProposal proposal "Prepush command failed" (Just logFileName)

    liftIO $ putStrLn . T.unpack $ "Updating: " <> fromBranchName ontoBranchName
    Git.checkout (LocalBranch ontoBranchName) -- in case script moved git

    if proposalDryRun proposal
    then do
        sendProposalEmail proposal "Dry-run: Prepush ran successfully" "" (Just logFileName)
    else do
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
usage = List.intercalate "\n"
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

    let proposedBranches =
            List.sort
            $ filter (\b -> proposedPrefix `T.isPrefixOf` (fromBranchName $ snd b)) remoteBranches

    liftIO $ mapM_ print proposedBranches
    forM_ (mapMaybe (\branch -> (branch,) <$> parseProposal (branchName branch)) (map (uncurry Git.RemoteBranch) proposedBranches))
           (uncurry $ attemptBranchOrAbort prepushCmd)



