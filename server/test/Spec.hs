{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Monad   (replicateM, void)
import           Test.QuickCheck

import qualified Data.Text       as T

import           Data.Char       (isAlphaNum)

import           Data.DeriveTH
import           Sling.Git
import           Sling.Lib
import           Sling.Proposal

-- ----------------------------------------------------------------------

prop_buildParseProposal :: Proposal -> Bool
prop_buildParseProposal p =
    case parseProposal $ formatProposal p of
        Nothing -> False
        Just p' -> p == p'

-- Arbitrary instances:

instance Arbitrary NonEmptyText where
    arbitrary = nonEmptyText . T.pack . getNonEmpty <$> arbitrary
    shrink = map (nonEmptyText . T.pack) . filter (not . null) . shrink . T.unpack . fromNonEmptyText

instance Arbitrary NatInt where
    arbitrary = NatInt . getNonNegative <$> arbitrary
    shrink = map NatInt . filter (>=0) . shrink . fromNatInt

hexDigits :: Gen Char
hexDigits = elements (['0'..'9'] ++ ['A'..'F'])

instance Arbitrary Hash where
    arbitrary = hash . T.pack <$> do
        l <- choose (1,40) -- this is your string length
        replicateM l hexDigits
    shrink = map (hash . T.pack) . filter (not . null) . shrink . T.unpack . fromHash

-- Determined 'empirically' :(
branchChars :: Gen Char
branchChars = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-/_\\.")

instance Arbitrary BranchName where
    arbitrary = mkBranchName . T.pack <$> do
        l <- choose (1,100)
        replicateM l branchChars
    shrink = map (mkBranchName . T.pack) . filter (not . null) . shrink . T.unpack . fromBranchName


hostNameChars :: Gen Char
hostNameChars = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_")

instance Arbitrary ServerId where
    arbitrary = ServerId . T.pack <$> do
        l <- choose (1,10)
        replicateM l hostNameChars

-- Determined 'empirically' :(
-- too restrictive?
remoteChars :: Gen Char
remoteChars = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_!@#$%&")

instance Arbitrary Remote where
    arbitrary = Remote . nonEmptyText . T.pack <$> do
        l <- choose (1,100)
        replicateM l remoteChars
    shrink (Remote x) = map Remote . shrink $ x

stupidEnsureNotEmpty :: String -> String
stupidEnsureNotEmpty [] = "X"
stupidEnsureNotEmpty xs = xs

instance Arbitrary Email where
    arbitrary = do
        user <- nonEmptyText . T.pack . stupidEnsureNotEmpty . filter (\x -> isAlphaNum x || x `elem` ".-_+") . getNonEmpty <$> arbitrary
        domain <- nonEmptyText . T.pack . stupidEnsureNotEmpty . filter (\x -> isAlphaNum x || x `elem` ".-") . getNonEmpty <$> arbitrary
        return $ Email user domain
    shrink (Email u d) = map (uncurry Email) $ shrink (u, d)

instance Arbitrary Prefix where
    arbitrary = Prefix <$> arbitrary

instance Arbitrary ProposalStatus where
    arbitrary = oneof
        [ pure ProposalProposed
        , pure ProposalRejected
        , ProposalInProgress <$> arbitrary
        ]

derive makeArbitrary ''Proposal
derive makeArbitrary ''Branch
derive makeArbitrary ''Ref

-- ----------------------------------------------------------------------

return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = void runTests


