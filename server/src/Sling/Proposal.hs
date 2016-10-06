{-# LANGUAGE OverloadedStrings #-}
module Sling.Proposal where

import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Sling.Git           as Git
import           Sling.Lib           (Email (..), NatInt, formatSepEmail,
                                      fromNatInt, fromNonEmptyText, hash,
                                      natInt, nonEmptyText, singleMatch,
                                      NonEmptyText, someText)
import           Control.Monad       (void)
import           Control.Applicative ((<|>))
import           Data.Monoid         ((<>))
import           Turtle              (Pattern, alphaNum, char, decimal, eof,
                                      hexDigit, notChar, oneOf, some, text)

data ProposalStatus
    = ProposalProposed
    | ProposalInProgress { _proposalInProgressServerId :: Text }
    | ProposalRejected
      deriving (Show, Eq, Ord)

emailPat :: Text -> Pattern Email
emailPat sep = do
    user <- nonEmptyText . T.pack <$> some (alphaNum <|> oneOf ".-_+")
    _ <- text sep
    domain <- nonEmptyText . T.pack <$> some (alphaNum <|> oneOf ".-")
    return $ Email user domain

newtype Prefix = Prefix { fromPrefix :: NonEmptyText }
      deriving (Show, Eq)

prefixToText :: Prefix -> Text
prefixToText = fromNonEmptyText . fromPrefix

prefixFromText :: Text -> Prefix
prefixFromText = Prefix . nonEmptyText

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

proposalPrefixPrefix :: Text
proposalPrefixPrefix = "prefix-"

formatProposal :: Proposal -> Text
formatProposal p = "sling/" <> prefix <> "/" <> suffix
    where
        prefix = maybe T.empty (\x -> (proposalPrefixPrefix <> prefixToText x) <> "/") (proposalPrefix p) <>
            case proposalStatus p of
                ProposalProposed -> proposedPrefix
                ProposalInProgress serverId -> inProgressPrefix <> "/" <> serverId
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
     <|> (flip Git.RefParent <$> ((natInt <$> decimal) <* text "#") <*> refPat))
     <|> (Git.RefHash . hash . T.pack <$> some hexDigit)
     <|> (text "R-" *> (Git.RefBranch <$> remoteBranchPat))
     <|> (text "L-" *> (Git.RefBranch . Git.LocalBranch <$> branchNamePat))
    where
        branchNamePat = Git.mkBranchName . T.pack <$> some (notChar '#')
        remoteBranchPat =
            Git.RemoteBranch <$> (Git.Remote . nonEmptyText . T.pack <$> some (notChar '/')) <*> (char '/' *> branchNamePat)


formatRef :: Git.Ref -> Text
formatRef (Git.RefParent r n) = (T.pack . show $ fromNatInt n) <> "#" <> formatRef r
formatRef (Git.RefBranch (Git.RemoteBranch r n)) = "R-" <> fromNonEmptyText (Git.remoteName r) <> "/" <> Git.fromBranchName n
formatRef r@(Git.RefBranch (Git.LocalBranch{})) = "L-" <> Git.refName r
formatRef r = Git.refName r

fieldSep :: Pattern ()
fieldSep = void $ char '/'

proposalPat :: Pattern Proposal
proposalPat = do
    _ <- text slingPrefix

    prefix <- (text ("/" <> proposalPrefixPrefix) *> (Just . Prefix . nonEmptyText <$> (someText <* fieldSep))) <|> (fieldSep *> pure Nothing)

    ps <- (text proposedPrefix *> pure ProposalProposed)
        <|> (text rejectBranchPrefix *> pure ProposalRejected)
        <|> (text inProgressPrefix *> fieldSep *> (ProposalInProgress <$> someText))
    fieldSep

    index <- natInt <$> decimal
    fieldSep

    name <- Git.mkBranchName <$> someText
    fieldSep

    _ <- text "base"
    fieldSep
    baseRef <- refPat
    fieldSep

    isDryRun <- (text ontoPrefix *> pure False) <|> (text dryRunOntoPrefix *> pure True)
    fieldSep

    ontoRef <- Git.mkBranchName <$> someText
    fieldSep

    _ <- text "user"
    fieldSep
    email <- emailPat "-at-"

    _ <- eof
    return $ Proposal email name baseRef ontoRef index ps isDryRun prefix

parseProposal :: Text -> Maybe Proposal
parseProposal = singleMatch proposalPat

slingPrefix :: Text
slingPrefix = "sling"

rejectBranchPrefix :: Text
rejectBranchPrefix = "rejected"

proposedPrefix :: Text
proposedPrefix = "proposed"

inProgressPrefix :: Text
inProgressPrefix = "in-progress"
