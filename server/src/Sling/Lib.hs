{-# LANGUAGE OverloadedStrings #-}
module Sling.Lib where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT, left, runEitherT, mapEitherT)
import           Data.Char (isSpace, isHexDigit)
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Turtle (ExitCode(..), Shell, sh, view, empty, procStrict
                        , Pattern, match, satisfy, some, anyChar)

-- Turtle stuff

type EShell a = EitherT ExitCode Shell a

eprocs :: Text -> [Text] -> EShell Text
eprocs cmd args = do
    (exitCode, t) <- procStrict cmd args empty
    case exitCode of
        ExitSuccess -> pure t
        _           -> do
            liftIO $ print (cmd, args)
            left exitCode

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
    liftIO $ putStrLn $ T.unpack msg
    left $ ExitFailure 1

runEShell :: EShell a -> IO ()
runEShell = sh . runEitherT

singleMatch :: Pattern b -> Text -> Maybe b
singleMatch m = safe head . match m

mustMatch :: Pattern b -> Text -> EShell b
mustMatch p t =
    case singleMatch p t of
        Nothing -> abort $ "Failed to parse: " <> t
        Just x -> return x

notSpace :: Pattern Char
notSpace = satisfy (not . isSpace)

ignoreError :: EShell () -> EShell ()
ignoreError act = do
    _ <- liftIO $ runEShell act
    return ()

someText :: Pattern Text
someText = T.pack <$> some anyChar

-- ----------------------------------------------------------------------

-- unenforced...
newtype NonEmptyText = NonEmptyText { fromNonEmptyText :: Text }
    deriving (Show, Eq, Ord)

instance IsString NonEmptyText where
    fromString = NonEmptyText . T.pack

-- TODO: :/
nonEmptyText t =
    if T.null t
    then error "Something has gone terribly wrong! Expected non-empty text."
    else NonEmptyText t

-- (very) poor man's natural number
newtype NatInt = NatInt { fromNatInt :: Int }
    deriving (Show, Eq, Ord)

natInt x =
    if x >= 0
    then NatInt x
    else error "Something has gone terribly wrong! Expected natural (integer >= 0)."

newtype Hash = Hash { fromHash :: Text }
    deriving (Show, Eq, Ord)

hash x =
    if T.all isHexDigit x || (T.length x < 1)
    then Hash x
    else error "Something has gone terribly wrong! Expected hex digits only."

data Email = Email { emailUser :: NonEmptyText, emailDomain :: NonEmptyText }
    deriving (Show, Eq)

formatSepEmail :: Text -> Email -> Text
formatSepEmail sep (Email user domain) = (fromNonEmptyText user) <> sep <> (fromNonEmptyText domain)

formatEmail :: Email -> Text
formatEmail = formatSepEmail "@"

