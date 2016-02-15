{-# LANGUAGE OverloadedStrings #-}
module Sling.Web
       ( CurrentState(..), emptyCurrentState
       , forkServer
       ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Monoid ((<>))
import qualified Data.Text as T
import Sling.Proposal (Proposal(..), formatProposal)
import Web.Spock.Simple
import System.IO (IOMode(..), withFile, hFileSize)
import System.IO.Error (tryIOError, isEOFError)
import Data.Time.Clock.POSIX (POSIXTime)
import Network.Wai (responseFile, FilePart(..))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy as L

data CurrentState =
    CurrentState
    { csPendingProposals :: [Proposal]
    , csCurrentProposal :: Maybe (Proposal, POSIXTime)
    , csCurrentLogFile :: Maybe FilePath
    }

emptyCurrentState :: CurrentState
emptyCurrentState =
    CurrentState
    { csPendingProposals = []
    , csCurrentProposal = Nothing
    , csCurrentLogFile = Nothing
    }

forkServer :: Int -> IO CurrentState -> IO ThreadId
forkServer port getCurrentState = forkIO (runServer port getCurrentState)

tailBytes :: Integer
tailBytes = 4096

htmlHead :: H.Html
htmlHead = H.head $ do
    H.link
        H.! A.rel "stylesheet"
        H.! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"

runServer :: Int -> IO CurrentState -> IO ()
runServer port getCurrentState =
    runSpock port $ spockT id $ do
        get "/" $ do
            state <- liftIO getCurrentState
            html $ L.toStrict $ renderHtml $ do
                htmlHead
                H.div H.! A.class_ "container" $ do
                    H.h1 "Sling Server"
                    H.h4 "Pending proposals"
                    if (length (csPendingProposals state) == 0)
                        then H.div (H.em $ H.text "none")
                        else H.ol $ do
                        forM_ (csPendingProposals state) $ \proposal -> do
                            H.li (H.text $ formatProposal proposal)
                    H.h4 "Current proposal"
                    case csCurrentProposal state of
                        Just (proposal, time) -> do
                            H.p . H.text $ formatProposal proposal
                            H.p . H.text $ "Started: " <> T.pack (show time)
                            H.p $ H.a H.! A.href "/log" $ "Tail of log..."
                        Nothing -> H.em $ H.text "none"

        get "/log" $ do
            state <- liftIO getCurrentState
            case csCurrentLogFile state of
                Nothing -> text "no current log"
                Just logFile -> do
                    res <- liftIO $ tryIOError $ withFile logFile ReadMode $ hFileSize
                    case res of
                        Left err ->
                            if isEOFError err
                            then return ()
                            else text $ "ERROR: " <> (T.pack $ show err)
                        Right size ->
                            response $ \status headers ->
                            responseFile status headers logFile
                            $ Just FilePart
                            { filePartOffset = max 0 (size - tailBytes)
                            , filePartByteCount = tailBytes
                            , filePartFileSize = size
                            }
