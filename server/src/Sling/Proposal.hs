{-# LANGUAGE OverloadedStrings #-}
module Sling.Proposal where

import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Sling.Git           as Git
import           Sling.Lib           (Email (..), Hash, NatInt, NonEmptyText,
                                      formatEmail, formatSepEmail, fromHash,
                                      fromNatInt, fromNonEmptyText, hash,
                                      natInt, nonEmptyText, singleMatch,
                                      someText)

import           Control.Applicative ((<|>))
import           Data.Monoid         ((<>))
import           Turtle              (Pattern, alphaNum, anyChar, char, decimal,
                                      digit, hexDigit, match, notChar, oneOf,
                                      satisfy, some, text)

data ProposalStatus = ProposalProposed | ProposalRejected
      deriving (Show, Eq, Ord)

emailPat :: Text -> Pattern Email
emailPat sep = do
    user <- nonEmptyText . T.pack <$> some (alphaNum <|> oneOf ".-_+")
    _ <- text sep
    domain <- nonEmptyText . T.pack <$> some (alphaNum <|> oneOf ".-")
    return $ Email user domain

data Proposal
    = Proposal
      { proposalEmail      :: Email
      , proposalName       :: NonEmptyText
      , proposalBranchBase :: Git.Ref
      , proposalBranchOnto :: Git.BranchName
      , proposalQueueIndex :: NatInt
      , proposalStatus     :: ProposalStatus
      }
      deriving (Show, Eq)

escape = T.replace "/" "\\/" . T.replace "\\" "\\\\"
unescape = T.replace "\\\\" "\\" . T.replace "\\/" "/"


formatProposal :: Proposal -> Text
formatProposal p = prefix <> suffix
    where
        prefix =
            case (proposalStatus p) of
                ProposalProposed -> proposedPrefix
                ProposalRejected -> rejectBranchPrefix
        suffix =
            escape (fromNonEmptyText $ proposalName p) <> "/"
            <> T.pack (show . fromNatInt . proposalQueueIndex $ p) <> "/"
            <> (formatRef $ proposalBranchBase p) <> "/onto/"
            <> (escape . Git.fromBranchName $ proposalBranchOnto p) <> "/"
            <> formatSepEmail "-at-" (proposalEmail p)

refPat :: Pattern Git.Ref
refPat =
    ((text "HEAD" >> pure Git.RefHead)
     <|> (flip Git.RefParent <$> ((natInt <$> decimal) <* text "^") <*> refPat))
     <|> (Git.RefHash . hash . T.pack <$> some hexDigit)
     <|> ((text "R-") *> (Git.RefBranch <$> remoteBranchPat))
     <|> ((text "L-") *> (Git.RefBranch . Git.LocalBranch <$> branchNamePat))
    where
        branchNamePat = Git.mkBranchName . unescape . T.pack <$> some (notChar '^')
        remoteBranchPat =
            Git.RemoteBranch <$> (Git.Remote . nonEmptyText . T.pack <$> some (notChar '/')) <*> (char '/' *> branchNamePat)


formatRef :: Git.Ref -> Text
formatRef (Git.RefParent r n) = (T.pack . show $ fromNatInt n) <> "^" <> formatRef r
formatRef (Git.RefBranch (Git.RemoteBranch r n)) = "R-" <> (escape $ fromNonEmptyText $ Git.remoteName r) <> "/" <> (escape $ Git.fromBranchName n)
formatRef r@(Git.RefBranch (Git.LocalBranch{})) = "L-" <> (escape $ Git.refName r)
formatRef r = escape $ Git.refName r

proposalPat :: Pattern Proposal
proposalPat = do
    ps <- (text proposedPrefix *> pure ProposalProposed) <|> (text rejectBranchPrefix *> pure ProposalRejected)
    name <- nonEmptyText . unescape <$> someText
    _ <- char '/'
    index <- natInt <$> decimal
    _ <- char '/'
    baseRef <- refPat
    _ <- text "/onto/"
    ontoRef <- Git.mkBranchName . unescape <$> someText
    _ <- char '/'
    email <- emailPat "-at-"
    return $ Proposal email name baseRef ontoRef index ps

parseProposal :: Text -> Maybe Proposal
parseProposal = singleMatch proposalPat

slingPrefix :: Text
slingPrefix = "sling"

rejectBranchPrefix :: Text
rejectBranchPrefix = slingPrefix <> "/rejected/"

proposedPrefix :: Text
proposedPrefix = slingPrefix <> "/proposed/"
