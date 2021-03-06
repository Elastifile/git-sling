{-# LANGUAGE OverloadedStrings #-}
module Sling.Lib where

import System.Exit (exitWith)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT, left, mapEitherT,
                                             runEitherT)
import Data.Char (isHexDigit, isSpace)
import Data.Monoid ((<>))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text                  as T
import           Turtle                     (ExitCode (..), Pattern, Shell,
                                             anyChar, empty, match, procStrict, proc,
                                             satisfy, sh, some, view)


import qualified System.IO

-- Turtle stuff

-- TODO: EShell a is a stream of a's, but throught sling we use it as
-- if it were IO a, which is wrong.
type EShell a = EitherT (Text, ExitCode) Shell a

eprintTo :: System.IO.Handle -> Text -> EShell ()
eprintTo x t = liftIO $ do
    System.IO.hPutStrLn x (T.unpack t)
    System.IO.hFlush x

eprint :: Text -> EShell ()
eprint = eprintTo System.IO.stderr

eproc :: Text -> [Text] -> Shell Text -> EShell ()
eproc cmd args input = do
    let cmdStr = T.intercalate " " (cmd:args)
    eprint $ "$ " <> cmdStr
    exitCode <- proc cmd args input
    case exitCode of
        ExitSuccess -> pure ()
        _           -> left ("Command failed: " <> cmdStr, exitCode)

eprocsIn :: Text -> [Text] -> Shell Text -> EShell Text
eprocsIn cmd args input = do
    let cmdStr = T.intercalate " " (cmd:args)
    eprint $ "$ " <> cmdStr
    (exitCode, t) <- procStrict cmd args input
    case exitCode of
        ExitSuccess -> pure t
        _           -> do
            eprint t
            left ("Command failed: " <> cmdStr, exitCode)

eprocs :: Text -> [Text] -> EShell Text
eprocs cmd args = eprocsIn cmd args empty

eprocsL :: Text -> [Text] -> EShell [Text]
eprocsL cmd args = T.lines <$> eprocs cmd args

wrapEither :: (Monad m) => (m a -> m b) -> m (Either e a) -> m (Either e b)
wrapEither f act = do
    a <- act
    case a of
        Left e' -> return $ Left e'
        Right a' -> Right <$> f (return a')

elift :: (Monad m) => (m a -> m b) -> EitherT e' m a -> EitherT e' m b
elift f = mapEitherT (wrapEither f)

eview :: Show a => EShell a -> EShell ()
eview = elift view

safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f xs = Just $ f xs

abort :: Text -> EShell a
abort msg = do
    eprint msg
    left (msg, ExitFailure 1)

runEShell :: EShell a -> IO ()
runEShell act = sh $ do
    res <- runEitherT act
    case res of
        Left (_, ExitSuccess) -> return ()
        Left (_msg, exitCode) -> liftIO (exitWith exitCode)
        _ -> return ()

singleMatch :: Pattern b -> Text -> Maybe b
singleMatch m = safe head . match m

mustMatch :: Pattern b -> Text -> EShell b
mustMatch p t =
    case singleMatch p t of
        Nothing -> abort $ "Failed to parse: '" <> t <> "'"
        Just x -> return x

notSpace :: Pattern Char
notSpace = satisfy (not . isSpace)

ignoreError :: EShell () -> EShell ()
ignoreError act = sh $ do
    _ <- runEitherT act
    return ()

someText :: Pattern Text
someText = T.pack <$> some anyChar

assert :: (a -> b -> Bool) -> a -> b -> Maybe Text -> EShell ()
assert f a b e = when (not $ f a b) $ abort ("ASSERTION FAILED: " <> maybe "" id e)

-- ----------------------------------------------------------------------

-- unenforced...
newtype NonEmptyText = NonEmptyText { fromNonEmptyText :: Text }
    deriving (Show, Eq, Ord)

instance IsString NonEmptyText where
    fromString = NonEmptyText . T.pack

-- TODO: :/
nonEmptyText :: Text -> NonEmptyText
nonEmptyText t =
    if T.null t
    then error "Something has gone terribly wrong! Expected non-empty text."
    else NonEmptyText t

-- (very) poor man's natural number
newtype NatInt = NatInt { fromNatInt :: Int }
    deriving (Show, Eq, Ord)

natInt :: Int -> NatInt
natInt x =
    if x >= 0
    then NatInt x
    else error "Something has gone terribly wrong! Expected natural (integer >= 0)."

newtype Hash = Hash { fromHash :: Text }
    deriving (Show, Eq, Ord)

hash :: Text -> Hash
hash x =
    if T.all isHexDigit x || (T.length x < 1)
    then Hash x
    else error "Something has gone terribly wrong! Expected hex digits only."

data Email = Email { emailUser :: NonEmptyText, emailDomain :: NonEmptyText }
    deriving (Show, Eq)

formatSepEmail :: Text -> Email -> Text
formatSepEmail sep (Email user domain) = fromNonEmptyText user <> sep <> fromNonEmptyText domain

formatEmail :: Email -> Text
formatEmail = formatSepEmail "@"

