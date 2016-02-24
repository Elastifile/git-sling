-- TODO
-- 1. take commits list from stdin (to allow arbitrary filtering, e.g. git log --author or git log -- path)
-- 2. constant number of workers
-- 3. assign work by 'commit distance function', either diff size of log distance
-- 4. each worker assigned constant (non-temp) directory, to reuse data between runs
-- 5. how to support workers on different machines?
-- 6. nicer output (e.g. bricks)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Monad (forM_, when)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.IORef
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Sling.Git                     (Branch (..), Ref (..))
import qualified Sling.Git as Git
import           Sling.Lib                     (EShell, abort,
                                                runEShell, ignoreError)
import qualified Data.List as List
import           Turtle ((&))

import           Control.Concurrent (forkIO)
import           Options.Applicative
import           System.Process (createProcess, proc, CreateProcess(..), StdStream(..), waitForProcess)
import           System.Directory (getCurrentDirectory, setCurrentDirectory, createDirectoryIfMissing)
import qualified System.Directory as Directory
import           System.Exit (ExitCode(..))
import           Control.Concurrent.MVar
import     Control.Concurrent.QSem
import qualified Data.Char as Char


split :: Foldable t => (a -> Bool) -> t a -> [[a]]
split p = foldr f' []
    where
        f' x [] = if p x then [] else [[x]]
        f' x (l:ls) = if p x then []:(l:ls) else (x:l):ls

chunks :: Int -> [a] -> [[a]]
chunks n xs
    | n < 1     = error "chunk size must be > 0"
    | n == 1    = [xs]
    | otherwise =
          case xs of
              [] -> []
              _ -> take n xs : chunks n (drop n xs)

headTail :: [t] -> Maybe (t, [t])
headTail [] = Nothing
headTail (cmd:args) = Just (cmd, args)

ceilDivInt :: (Integral b, Integral a, Integral c) => a -> b -> c
ceilDivInt x y = ceiling $ (fromIntegral x :: Double) / fromIntegral y

usage :: String
usage = List.intercalate "\n"
    [ "Usage: parallel-build --base COMMIT --head COMMIT -- COMMAND..."
    , ""
    , "where COMMAND is the command to run on each attempted commit."
    ]

data Options =
    Options
    { optBaseCommit :: Git.Ref
    , optHeadCommit :: Git.Ref
    , optGitDir :: FilePath
    , optParallelism :: Int
    , optCommandAndArgs :: [String]
    }


parseOpts :: IO Options
parseOpts = execParser $
    info (helper <*> parser)
    (fullDesc <> header "parallel-build - run a command on several commits in parallel")

parseRef :: String -> Ref
parseRef = RefBranch . LocalBranch . Git.mkBranchName . T.pack

parser :: Parser Options
parser = Options
    <$> (parseRef <$> strOption
         (long "base" <>
          metavar "COMMIT" <>
          help "Base commit for range"))
    <*> (parseRef <$> strOption
         (long "head" <>
          metavar "COMMIT" <>
          help "Head commit for range"))
    <*> strOption
         (long "git-dir" <>
          metavar "PATH" <>
          help "Path to .git dir of repository to work on")
    <*> option auto
          (value 1 <>
           short 'p' <>
           long "parallelism" <>
           metavar "NUM" <>
           help "Work on this number of commits simultaneously.")
    <*> some (argument str
         (metavar "-- COMMAND" <>
          help "Command to run on each commit (non-zero exit code will abort entire run)"))

data BuildState =
    BuildState
    { bsChunksLeft :: IORef Int
    , bsCurrentDirLock :: MVar () -- TODO: Horrid.
    }

withCurrentDirectory :: MonadIO io => MVar () -> FilePath -> io a -> io a
withCurrentDirectory mv path act = do
    liftIO $ takeMVar mv
    cwd' <- liftIO getCurrentDirectory
    liftIO $ setCurrentDirectory path
    res <- act
    liftIO $ setCurrentDirectory cwd'
    liftIO $ putMVar mv ()
    return res

buildCommits :: BuildState -> FilePath -> (String, [String]) -> [Git.LogEntry] -> EShell ()
buildCommits state origGitPath (cmd, args) logEntries = do
    let dirLabel = filter (\x -> Char.isAlphaNum x || x == '_') $ List.intercalate "_" (cmd : args)
    dirFullPath <- liftIO $ Directory.makeAbsolute $ mconcat [ origGitPath, "/par-build.", dirLabel ]
    liftIO $ do
        putStrLn $ "Creating dir: " ++ dirFullPath
        createDirectoryIfMissing True dirFullPath
    let inDir = withCurrentDirectory (bsCurrentDirLock state) dirFullPath
    Git.clone (T.pack origGitPath) dirFullPath (Just 1) Nothing & ignoreError -- TODO
    inDir Git.fetch
    forM_ logEntries $ \logEntry -> do
        inDir $ Git.checkout (Git.RefHash $ Git.logEntryShortHash logEntry)
        (_, _, _, procHandle) <-
            liftIO $ createProcess (proc cmd args){ cwd = Just dirFullPath
                                                  , std_in = CreatePipe }
        exitCode <- liftIO $ waitForProcess procHandle
        case exitCode of
            ExitFailure n -> error $ "Process failed with code: " ++ show n
            _ -> return ()
--    eprocs "rm" ["-rf", T.pack dirFullPath] >> pure ()
    return ()

main :: IO ()
main = runEShell $ do
    options <- liftIO parseOpts
    cmds <-
        case optCommandAndArgs options of
            [] -> abort "Need at least one command to run!"
            cmds -> return
                $ mapMaybe headTail
                $ split (== ";") cmds

    dirLock <- liftIO $ newMVar ()
    commits <- reverse <$> Git.log (optBaseCommit options) (optHeadCommit options)
    let chunkSize = (length commits * length cmds) `ceilDivInt` optParallelism options
        commitChunks = chunks chunkSize commits
        totalCmdInvocations = (length commitChunks * length cmds)
    chunksRef <- liftIO $ newIORef totalCmdInvocations
    let state = BuildState chunksRef dirLock
    done <- liftIO newEmptyMVar
    sem <- liftIO $ newQSem $ optParallelism options
    forM_ commitChunks $ \chunk -> do
        liftIO $ putStrLn $ "Chunk starting. Length = " ++ show (length chunk)
        _threadId <- liftIO $ forkIO $
            forM_ cmds $ \cmd -> do
                -- TODO: Use bracket
                liftIO $ waitQSem sem
                _threadId2 <- liftIO $ forkIO $ do
                    runEShell $ buildCommits state (optGitDir options) cmd chunk
                    remaining <- atomicModifyIORef (bsChunksLeft state) (\x -> let r = x - 1 in (r, r))
                    putStrLn $ "Chunk done for command: " ++ show cmd
                    putStrLn $ "Done: " ++ show (totalCmdInvocations - remaining) ++ "/" ++ show totalCmdInvocations
                    when (remaining == 0) $ do
                        putStrLn "Last chunk / command finished, signalling main thread..."
                        putMVar done ()
                    signalQSem sem
                return ()
        return ()
    liftIO $ do
        putStrLn "Waiting for workers..."
        takeMVar done
        putStrLn "All chunks / commands finished."
