{-# LANGUAGE OverloadedStrings #-}
module Sling.Web
       ( CurrentState(..), emptyCurrentState
       , forkServer
       ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Sling.Proposal (Proposal(..), formatProposal)
import Web.Spock.Simple
import System.IO (IOMode(..), withFile, hFileSize)
import System.IO.Error (tryIOError, isEOFError)
import Network.Wai (responseFile, FilePart(..))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Data.Text.Lazy as L

data CurrentState =
    CurrentState
    { csPendingProposals :: [Proposal]
    , csCurrentProposal :: Maybe Proposal
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

runServer :: Int -> IO CurrentState -> IO ()
runServer port getCurrentState =
    runSpock port $ spockT id $ do
        get "/" $ do
            state <- liftIO getCurrentState
            html $ L.toStrict $ renderHtml $ do
                H.h1 "Sling Server"
                H.div "Pending proposals:"
                forM_ (csPendingProposals state) $ \proposal -> do
                    H.div (H.text $ formatProposal proposal)
                H.div $ H.text $ "Current proposal:" <> (fromMaybe "<idle>" $ formatProposal <$> csCurrentProposal state)
        get "/log" $ do
            state <- liftIO getCurrentState
            case csCurrentLogFile state of
                Nothing -> text "<no current log>"
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
