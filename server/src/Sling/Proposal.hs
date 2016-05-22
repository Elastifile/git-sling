{-# LANGUAGE OverloadedStrings #-}
module Sling.Proposal where

import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Sling.Git           as Git
import           Sling.Lib           (Email (..), NatInt, formatSepEmail,
                                      fromNatInt, fromNonEmptyText, hash,
                                      natInt, nonEmptyText, singleMatch,
                                      someText)

import           Control.Applicative ((<|>))
import           Data.Monoid         ((<>))
import           Turtle              (Pattern, alphaNum, char, decimal, eof,
                                      hexDigit, notChar, oneOf, some, text)

data ProposalStatus = ProposalProposed | ProposalRejected
      deriving (Show, Eq, Ord)

emailPat :: Text -> Pattern Email
emailPat sep = do
    user <- nonEmptyText . T.pack <$> some (alphaNum <|> oneOf ".-_+")
    _ <- text sep
    domain <- nonEmptyText . T.pack <$> some (alphaNum <|> oneOf ".-")
    return $ Email user domain

newtype Prefix = Prefix { fromPrefix :: Text }
      deriving (Show, Eq)

data Proposal
    = Proposal
      { proposalEmail      :: Email
      , proposalName       :: Git.BranchName -- not really a branch, but it will be used as a branch name
      , proposalBranchBase :: Git.Ref
      , proposalBranchOnto :: Git.BranchName
      , proposalQueueIndex :: NatInt
      , proposalStatus     :: ProposalStatus
      , proposalDryRun     :: Bool
      , proposalPrefix     :: Maybe Prefix
      }
      deriving (Show, Eq)

ontoPrefix :: Text
ontoPrefix = "onto"

dryRunOntoPrefix :: Text
dryRunOntoPrefix = "dry-run-onto"

formatProposal :: Proposal -> Text
formatProposal p = "sling/" <> prefix <> "/" <> suffix
    where
        prefix = maybe T.empty (\x -> fromPrefix x <> "/") (proposalPrefix p) <>
            case proposalStatus p of
                ProposalProposed -> proposedPrefix
                ProposalRejected -> rejectBranchPrefix
        suffix =
            T.pack (show . fromNatInt . proposalQueueIndex $ p)
            <> "/" <> Git.fromBranchName (proposalName p)
            <> "/base/" <> formatRef (proposalBranchBase p)
            <> "/" <> (if proposalDryRun p then dryRunOntoPrefix else ontoPrefix)  <> "/"
            <> Git.fromBranchName (proposalBranchOnto p)
            <> "/user/" <> formatSepEmail "-at-" (proposalEmail p)

refPat :: Pattern Git.Ref
refPat =
    ((text "HEAD" >> pure Git.RefHead)
     <|> (flip Git.RefParent <$> ((natInt <$> decimal) <* text "^") <*> refPat))
     <|> (Git.RefHash . hash . T.pack <$> some hexDigit)
     <|> (text "R-" *> (Git.RefBranch <$> remoteBranchPat))
     <|> (text "L-" *> (Git.RefBranch . Git.LocalBranch <$> branchNamePat))
    where
        branchNamePat = Git.mkBranchName . T.pack <$> some (notChar '^')
        remoteBranchPat =
            Git.RemoteBranch <$> (Git.Remote . nonEmptyText . T.pack <$> some (notChar '/')) <*> (char '/' *> branchNamePat)


formatRef :: Git.Ref -> Text
formatRef (Git.RefParent r n) = (T.pack . show $ fromNatInt n) <> "^" <> formatRef r
formatRef (Git.RefBranch (Git.RemoteBranch r n)) = "R-" <> fromNonEmptyText (Git.remoteName r) <> "/" <> Git.fromBranchName n
formatRef r@(Git.RefBranch (Git.LocalBranch{})) = "L-" <> Git.refName r
formatRef r = Git.refName r

proposalPat :: (Maybe Prefix) -> Pattern Proposal
proposalPat mPrefix = do
    _ <- text "sling/"
    case mPrefix of
        Nothing -> return ()
        Just p -> do
            _ <- text $ fromPrefix p
            _ <- char '/'
            return ()
    ps <- (text proposedPrefix *> pure ProposalProposed) <|> (text rejectBranchPrefix *> pure ProposalRejected)
    _ <- char '/'
    index <- natInt <$> decimal
    _ <- char '/'
    name <- Git.mkBranchName <$> someText
    _ <- text "/base/"
    baseRef <- refPat
    _ <- char '/'
    isDryRun <- (text ontoPrefix *> pure False) <|> (text dryRunOntoPrefix *> pure True)
    _ <- char '/'
    ontoRef <- Git.mkBranchName <$> someText
    _ <- text "/user/"
    email <- emailPat "-at-"
    _ <- eof
    return $ Proposal email name baseRef ontoRef index ps isDryRun mPrefix

parseProposal :: Maybe Prefix -> Text -> Maybe Proposal
parseProposal prefix = singleMatch (proposalPat prefix)

slingPrefix :: Text
slingPrefix = "sling"

rejectBranchPrefix :: Text
rejectBranchPrefix = "rejected"

proposedPrefix :: Text
proposedPrefix = "proposed"

