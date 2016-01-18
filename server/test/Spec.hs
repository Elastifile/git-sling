{-# LANGUAGE TemplateHaskell #-}
import           Control.Monad (replicateM)
import           Test.QuickCheck

import qualified Data.Text as T

import           Data.Char (isAlphaNum)

import           Sling.Proposal
import           Sling.Git
import           Sling.Lib
import           Data.DeriveTH

-- ----------------------------------------------------------------------

prop_buildParseProposal :: Proposal -> Bool
prop_buildParseProposal p =
    case (parseProposal $ formatProposal p) of
        Nothing -> False
        Just p' -> p == p'

-- Arbitrary instances:

instance Arbitrary NonEmptyText where
    arbitrary = nonEmptyText . T.pack . getNonEmpty <$> arbitrary
    shrink = map (nonEmptyText . T.pack) . filter (not . null) . shrink . T.unpack . fromNonEmptyText

instance Arbitrary NatInt where
    arbitrary = NatInt . getNonNegative <$> arbitrary
    shrink = map NatInt . filter (>=0) . shrink . fromNatInt

hexDigits = elements (['0'..'9'] ++ ['A'..'F'])

instance Arbitrary Hash where
    arbitrary = hash . T.pack <$> do
        l <- choose (1,40) -- this is your string length
        replicateM l hexDigits
    shrink = map (hash . T.pack) . filter (not . null) . shrink . T.unpack . fromHash

-- Determined 'empirically' :(
-- TODO: this is too permissive. See 'man git-check-ref-format' or
-- https://www.kernel.org/pub/software/scm/git/docs/git-check-ref-format.html
branchChars = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-/_\\.!@#$%&")

instance Arbitrary BranchName where
    arbitrary = mkBranchName . T.pack <$> do
        l <- choose (1,100)
        replicateM l branchChars
    shrink = map (mkBranchName . T.pack) . filter (not . null) . shrink . T.unpack . fromBranchName

-- Determined 'empirically' :(
-- too restrictive?
remoteChars = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_!@#$%&")

instance Arbitrary Remote where
    arbitrary = Remote . nonEmptyText . T.pack <$> do
        l <- choose (1,100)
        replicateM l remoteChars
    shrink (Remote x) = map Remote . shrink $ x

stupidEnsureNotEmpty [] = "X"
stupidEnsureNotEmpty xs = xs

instance Arbitrary Email where
    arbitrary = do
        user <- nonEmptyText . T.pack . stupidEnsureNotEmpty . filter (\x -> isAlphaNum x || x `elem` ".-_+") . getNonEmpty <$> arbitrary
        domain <- nonEmptyText . T.pack . stupidEnsureNotEmpty . filter (\x -> isAlphaNum x || x `elem` ".-") . getNonEmpty <$> arbitrary
        return $ Email user domain
    shrink (Email u d) = map (uncurry Email) $ shrink (u, d)

derive makeArbitrary ''ProposalStatus
derive makeArbitrary ''Proposal

derive makeArbitrary ''Branch
derive makeArbitrary ''Ref

-- ----------------------------------------------------------------------

return []
runTests = $quickCheckAll

main :: IO ()
main = runTests >> return ()


