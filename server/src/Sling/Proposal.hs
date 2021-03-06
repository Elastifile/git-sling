{-# LANGUAGE OverloadedStrings #-}
module Sling.Proposal
    ( Proposal (..)
    , ProposalStatus (..)
    , ServerId (..)
    , MergeType (..)
    , ProposalType (..)
    , Prefix (..)
    , prefixFromText
    , prefixToText
    , toBranchName
    , fromBranchName
    , formatProposal
    , parseProposal
    ) where

import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Sling.Git           as Git
import           Sling.Lib           (Email (..), NatInt, formatSepEmail,
                                      fromNatInt, fromNonEmptyText, Hash, fromHash, hash,
                                      natInt, nonEmptyText, singleMatch,
                                      NonEmptyText, someText)
import           Control.Monad       (void)
import           Control.Applicative ((<|>))
import           Data.Monoid         ((<>))
import           Turtle              (Pattern, alphaNum, char, decimal, eof,
                                      hexDigit, notChar, oneOf, some, text)

newtype ServerId = ServerId { fromServerId :: Text }
      deriving (Show, Eq, Ord)

data ProposalStatus
    = ProposalProposed
    | ProposalInProgress { _proposalInProgressServerId :: ServerId }
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

data MergeType = MergeTypeFlat | MergeTypeKeepMerges
      deriving (Show, Eq, Ord)

data ProposalType
    = ProposalTypeRebase { _ptBranchToRebase :: Git.BranchName }
    | ProposalTypeMerge { _ptMergeType :: MergeType,
                          _ptBase :: Hash }
      deriving (Show, Eq, Ord)

data Proposal
    = Proposal
      { proposalEmail      :: Email
      , proposalName       :: Git.BranchName -- not really a branch, but it will be used as a branch name
      , proposalType       :: ProposalType
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

slashEscape :: Text
slashEscape = ","

formatBranchName :: Git.BranchName -> Text
formatBranchName = T.replace "/" slashEscape . Git.fromBranchName

branchNamePat :: Pattern Git.BranchName
branchNamePat = Git.mkBranchName . T.replace slashEscape "/" . T.pack <$> some (notChar '#')

proposalTypeMergePrefix :: Text
proposalTypeMergePrefix = "base"

proposalTypeRebasePrefix :: Text
proposalTypeRebasePrefix = "rebase"

mergeTypePrefix :: MergeType -> Text
mergeTypePrefix MergeTypeKeepMerges = "-keep"
mergeTypePrefix MergeTypeFlat = ""

formatProposalType :: ProposalType -> Text
formatProposalType (ProposalTypeMerge mergeType ref) = proposalTypeMergePrefix <> mergeTypePrefix mergeType <> "/" <> fromHash ref
formatProposalType (ProposalTypeRebase name) = proposalTypeRebasePrefix <> "/" <> (formatBranchName name)

toBranchName :: Proposal -> Git.BranchName
toBranchName = Git.mkBranchName . formatProposal

formatProposal :: Proposal -> Text
formatProposal p = "sling/" <> prefix <> "/" <> suffix
    where
        prefix = maybe T.empty (\x -> (proposalPrefixPrefix <> prefixToText x) <> "/") (proposalPrefix p) <>
            case proposalStatus p of
                ProposalProposed -> proposedPrefix
                ProposalInProgress serverId -> inProgressPrefix <> "/" <> fromServerId serverId
                ProposalRejected -> rejectBranchPrefix
        suffix =
            T.pack (show . fromNatInt . proposalQueueIndex $ p)
            <> "/" <> formatBranchName (proposalName p)
            <> "/" <> formatProposalType (proposalType p)
            <> "/" <> (if proposalDryRun p then dryRunOntoPrefix else ontoPrefix)
            <> "/" <> formatBranchName (proposalBranchOnto p)
            <> "/user/" <> formatSepEmail "-at-" (proposalEmail p)

hashPat :: Pattern Hash
hashPat = hash . T.pack <$> some hexDigit

formatRef :: Git.Ref -> Text
formatRef (Git.RefParent r n) = (T.pack . show $ fromNatInt n) <> "#" <> formatRef r
formatRef (Git.RefBranch (Git.RemoteBranch r n)) = "R-" <> fromNonEmptyText (Git.remoteName r) <> "/" <> formatBranchName n
formatRef r@(Git.RefBranch (Git.LocalBranch{})) = "L-" <> Git.refName r
formatRef r = Git.refName r

fieldSep :: Pattern ()
fieldSep = void $ char '/'

mergeTypePat :: Pattern MergeType
mergeTypePat = (text (mergeTypePrefix MergeTypeKeepMerges) *> pure MergeTypeKeepMerges)
    <|> (pure MergeTypeFlat)

movePat :: Pattern ProposalType
movePat = (text proposalTypeMergePrefix *> (ProposalTypeMerge <$> mergeTypePat <*> (fieldSep *> hashPat)))
    <|> (text proposalTypeRebasePrefix *> fieldSep *> (ProposalTypeRebase <$> branchNamePat))

proposalPat :: Pattern Proposal
proposalPat = do
    _ <- text slingPrefix

    prefix <- (text ("/" <> proposalPrefixPrefix) *> (Just . Prefix . nonEmptyText <$> (someText <* fieldSep))) <|> (fieldSep *> pure Nothing)

    ps <- (text proposedPrefix *> pure ProposalProposed)
          <|> (text rejectBranchPrefix *> pure ProposalRejected)
          <|> (text inProgressPrefix *> fieldSep *> (ProposalInProgress . ServerId <$> someText))
    fieldSep

    index <- natInt <$> decimal
    fieldSep

    name <- branchNamePat
    fieldSep

    moveBranch <- movePat
    fieldSep

    isDryRun <- (text ontoPrefix *> pure False) <|> (text dryRunOntoPrefix *> pure True)
    fieldSep

    ontoRef <- branchNamePat
    fieldSep

    _ <- text "user"
    fieldSep
    email <- emailPat "-at-"

    _ <- eof
    return $ Proposal email name moveBranch ontoRef index ps isDryRun prefix

parseProposal :: Text -> Maybe Proposal
parseProposal = singleMatch proposalPat

fromBranchName :: Git.BranchName -> Maybe Proposal
fromBranchName = parseProposal . Git.fromBranchName

slingPrefix :: Text
slingPrefix = "sling"

rejectBranchPrefix :: Text
rejectBranchPrefix = "rejected"

proposedPrefix :: Text
proposedPrefix = "proposed"

inProgressPrefix :: Text
inProgressPrefix = "in-progress"
