{-# LANGUAGE OverloadedStrings #-}
module Sling.Email
    ( formatCommitsForEmail
    , sendProposalEmail
    , EmailType(..), IncludeAttachment(..)
    ) where

import           Control.Monad (void, when, join)
import           Control.Monad.Except (MonadError (..))
import           Data.String (fromString)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as LE
import           Data.Monoid ((<>))
import qualified Network.Mail.Mime as Mail
import           System.IO.Error (tryIOError)
import           System.IO (withFile, hSeek, SeekMode(..), IOMode(..), hFileSize)
import           Text.Blaze.Html (toHtml, (!))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Control.Concurrent (forkIO)
import           Turtle (echo)
import           Network.BSD (getHostName)

import qualified Sling.Git as Git
import           Sling.Lib                     (EShell, Email (..), fromHash,
                                                eprocsIn, eprocsL, formatEmail, eprint,
                                                runEShell)
import           Sling.Path (encodeFP)
import           Sling.Prepush (PrepushLogs(..))
import           Sling.Options (Options(..), isDryRun)
import           Sling.Proposal (Proposal(..), formatProposal)
import           Filesystem.Path.CurrentOS (FilePath)

import           Prelude hiding (FilePath)

data IncludeAttachment = FullAttachment | MinimalAttachment
data EmailType = ProposalSuccessEmail | ProposalFailureEmail | ProposalAttemptEmail

sourceEmail :: Email
sourceEmail = Email "elasti-prepush" "elastifile.com"


addAttachment :: FilePath -> Text -> IO (Maybe Mail.Part)
addAttachment fn attachedFN = do
    res <- liftIO $ tryIOError $ withFile (T.unpack $ encodeFP fn) ReadMode $
        \handle -> do
            size <- hFileSize handle
            -- SeekFromEnd seems broken
            hSeek handle AbsoluteSeek (max 0 (size - (1024*1024)))
            BS8.hGetContents handle
    case res of
        Left err -> do
            echo . T.pack $ "Failed reading from file: " <> show fn <> ", error: " <> show err
            return Nothing
        Right contents
            | BS8.length contents == 0 -> do
                  echo . T.pack $ "Empty data in file: " <> show fn
                  return Nothing
            | otherwise ->
                  return $ Just
                  $ Mail.Part "text/html; charset=utf-8" Mail.QuotedPrintableText (Just attachedFN) []
                  (LE.encodeUtf8 ("<html><body><pre>" <> renderHtml (toHtml $ TE.decodeUtf8 contents) <> "</pre></body></html>"))


addAttachments :: Maybe PrepushLogs -> IncludeAttachment -> Mail.Mail -> EShell Mail.Mail
addAttachments prepushLogs includeAttachment mail =
    case prepushLogs of
        Nothing -> return mail
        Just (PrepushLogs logDir fullLogOutputFile) -> do
            logPart <- liftIO $ addAttachment fullLogOutputFile "logtail.html"
            tarParts <- case includeAttachment of
                MinimalAttachment -> return []
                FullAttachment -> do
                    let tarName = encodeFP logDir <> ".tgz"
                    _tarOut <-
                        eprocsL "bash" ["-o", "pipefail", "-c",
                                        "tar czvf " <> tarName <> " " <> encodeFP logDir]
                    content <- liftIO $ LBS.readFile $ T.unpack tarName
                    return [Mail.Part "application/gzip" Mail.Base64 (Just tarName) [] content]
            return $ Mail.addPart (maybe id (:) logPart tarParts) mail

htmlFormatCommit :: Maybe Text -> Git.LogEntry -> H.Html
htmlFormatCommit urlPrefix l = do
    let hashCol =
            case join $ Git.githubCommitUrl (Git.logEntryFullHash l) <$> urlPrefix of
                Nothing -> fromString . T.unpack $ fromHash $ Git.logEntryShortHash l
                Just url -> H.a ! A.href (fromString $ T.unpack url) $ fromString . T.unpack $ fromHash $ Git.logEntryShortHash l
    H.td hashCol
    H.td $ fromString $ T.unpack $ Git.logEntryAuthor l
    H.td $ fromString $ T.unpack $ Git.logEntryTitle l

htmlFormatCommitLog :: [Git.LogEntry] -> Maybe Text -> H.Html
htmlFormatCommitLog commits urlPrefix = do
    H.p "Commits:"
    H.table . H.tbody $ mapM_ (H.tr . htmlFormatCommit urlPrefix) commits

formatCommitsForEmail :: Options -> Proposal -> [Git.LogEntry] -> Maybe Text -> H.Html
formatCommitsForEmail options proposal commits baseUrl = do
    H.p . H.b $ "Proposal"
    H.p . fromString $ "Proposed by: " <> T.unpack (formatEmail . proposalEmail $ proposal)
    H.p $ do
        H.span "Onto branch: "
        H.b (fromString . T.unpack . Git.fromBranchName . proposalBranchOnto $ proposal)
        when (isDryRun options proposal) $ H.span " (Dry run only, branch not moved)"
    htmlFormatCommitLog commits baseUrl

formatProposalEmail :: Options -> Proposal -> Text -> H.Html -> Maybe PrepushLogs -> IncludeAttachment -> EmailType -> EShell LBS.ByteString
formatProposalEmail options proposal subject body prepushLogs includeAttachment emailType = do
    webHref <- liftIO $ (<> (":" <> show (optWebServerPort options))) . ("http://" <>) <$> getHostName
    let html = renderHtml $ do
            H.p . H.b $ fromString $ T.unpack subject
            body
            case emailType of
                ProposalAttemptEmail -> H.p $ H.a H.! A.href (H.preEscapedToValue webHref)  $ "Sling Server Status"
                ProposalFailureEmail -> return ()
                ProposalSuccessEmail -> return ()

    mail1 <- liftIO $ Mail.simpleMail
        (Mail.Address Nothing $ formatEmail $ proposalEmail proposal)
        (Mail.Address Nothing $ formatEmail sourceEmail)
        ((if isDryRun options proposal then "(dry run) " else "")
         <> "#" <> (T.pack . show $ proposalQueueIndex proposal)
         <> " " <> Git.fromBranchName (proposalName proposal)
         <> " onto " <> Git.fromBranchName (proposalBranchOnto proposal) <> "")
        ""
        html
        []

    mail <- case emailType of
        ProposalSuccessEmail -> return mail1
        ProposalAttemptEmail -> return mail1
        ProposalFailureEmail -> addAttachments prepushLogs includeAttachment mail1

    liftIO $ Mail.renderMail' mail

sendProposalEmail :: Options -> Proposal -> Text -> H.Html -> Maybe PrepushLogs -> EmailType -> EShell ()
sendProposalEmail options proposal subject body prepushLogs emailType = do
    let sendEmailWith includeAttachment = do
            renderdBS <- formatProposalEmail options proposal subject body prepushLogs includeAttachment emailType
            eprocsIn (head $ optEmailClient options) (tail (optEmailClient options) ++ [formatEmail $ proposalEmail proposal]) $ return (L.toStrict $ LE.decodeUtf8 renderdBS)

    eprint $ "Sending email (async) to: " <> formatEmail (proposalEmail proposal) <> " with subject: " <> subject
    -- TODO turtle is terrible at interleaving the outputs here. Better to replace with non-turtle stuff.
    liftIO $ void $ forkIO $ runEShell $ do
        sendEmailWith FullAttachment
        `catchError`
        \e -> do
                eprint $ "Error while sending with attachment, will attempt to send without attachment: " <> (T.pack $ show e)
                sendEmailWith MinimalAttachment

    return ()

