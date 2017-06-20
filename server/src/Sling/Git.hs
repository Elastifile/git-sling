{-# LANGUAGE OverloadedStrings #-}
module Sling.Git where

import           Control.Applicative    (optional, (<|>))
import           Control.Monad          (join, when)
import           Control.Monad.Except   (MonadError (..))

import           Data.Maybe             (mapMaybe, catMaybes)
import           Data.Monoid            ((<>))
import           Data.String            (IsString (..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Sling.Lib
import           Turtle                 (Pattern, anyChar, char, eof, hexDigit,
                                         notChar, selfless, some, space, spaces,
                                         text)

git :: [Text] -> EShell [Text]
git args = eprocsL "git" args

data FileStatus = Unmodified | Modified | Added | Deleted | Renamed | Copied | Unmerged
    deriving (Show, Eq, Ord)

data IndexStatus
    = Tracked { _trackedStaged   :: FileStatus
              , _trackedUnstaged :: FileStatus }
    | Untracked
    | Ignored
    deriving (Show, Eq, Ord)

data IndexedName = Name { _indexedName :: NonEmptyText }
                 | Rename { _indexedOldName :: NonEmptyText
                          , _indexedNewName :: NonEmptyText }
    deriving (Show, Eq, Ord)

data GitStatus = GitStatus { statusIndex    :: IndexStatus
                           , statusFileName :: IndexedName }
    deriving (Show, Eq, Ord)

newtype Remote = Remote { remoteName :: NonEmptyText }
    deriving (Show, Eq, Ord)

newtype BranchName = BranchName { fromBranchName :: Text }
    deriving (Show, Eq, Ord)

mkBranchName :: Text -> BranchName
mkBranchName = BranchName -- TODO validate

instance IsString BranchName where
    fromString = BranchName . fromString

data Branch
    = LocalBranch { _localBranchName :: BranchName }
    | RemoteBranch { _remoteBranchRemote :: Remote
                   , _remoteBranchName   :: BranchName }
    deriving (Show, Eq, Ord)

data Ref = RefBranch Branch | RefHead | RefHash Hash | RefParent Ref NatInt
    deriving (Show, Eq, Ord)

refTraverseHash :: Applicative f => (Hash -> f Hash) -> Ref -> f Ref
refTraverseHash _ (RefBranch b) = pure $ RefBranch b
refTraverseHash _ RefHead = pure RefHead
refTraverseHash f (RefHash h) = RefHash <$> f h
refTraverseHash f (RefParent r n) = flip RefParent n <$> refTraverseHash f r

branchName :: Branch -> BranchName
branchName (LocalBranch n) = n
branchName (RemoteBranch _ n) = n

branchFullName :: Branch -> Text
branchFullName (LocalBranch n) = fromBranchName n
branchFullName (RemoteBranch (Remote r) n) = fromNonEmptyText r <> "/" <> fromBranchName n

refName :: Ref -> Text
refName (RefBranch b) = branchFullName b
refName RefHead = "HEAD"
refName (RefHash t) = fromHash t
refName (RefParent r n) = refName r <> "~" <> T.pack (show $ fromNatInt n)

fileStatusPat :: Pattern FileStatus
fileStatusPat = (" " *> pure Unmodified)
                 <|> ("M" *> pure Modified)
                 <|> ("A" *> pure Added)
                 <|> ("D" *> pure Deleted)
                 <|> ("R" *> pure Renamed)
                 <|> ("C" *> pure Copied)
                 <|> ("U" *> pure Unmerged)

trackedPat :: Pattern IndexStatus
trackedPat = Tracked <$> fileStatusPat <*> fileStatusPat

indexStatusPat :: Pattern IndexStatus
indexStatusPat = ("??" *> pure Untracked)
                  <|> "!!" *> pure Ignored
                  <|> trackedPat

fileNamePat :: Pattern Text
fileNamePat = T.pack <$> some notSpace -- todo handle escaping

filePat :: Pattern GitStatus
filePat = do _ <- spaces
             s <- indexStatusPat
             _ <- some space
             fileName <- ((Name . nonEmptyText <$> fileNamePat) <* eof)
                          <|> (Rename . nonEmptyText <$> (fileNamePat <* text " -> ") <*> (nonEmptyText <$> fileNamePat <* eof))
             return $ GitStatus s fileName

status :: EShell [Maybe GitStatus]
status = map (singleMatch filePat) <$> git ["status", "--porcelain"]
    -- ^|^ rmap (singleMatch filePat)

shortenHash :: Hash -> EShell Hash
shortenHash h = hash . head <$> git ["rev-parse", "--short", fromHash h]

shortenRef :: Ref -> EShell Ref
shortenRef = refTraverseHash shortenHash

branchToHash :: Branch -> EShell Hash
branchToHash b = hash . head <$> git ["rev-parse", "--short", branchFullName b]

class CmdLineOption c where
    optionToText :: c -> Text

fetch :: EShell ()
fetch = git ["fetch", "-p"] >> pure ()

remote :: EShell [Remote]
remote = map (Remote . nonEmptyText) <$> git ["remote"]

clone :: Text -> FilePath -> Maybe Int -> Maybe Hash -> EShell ()
clone source target depth hash' =
    git (concat
         [ ["clone"]
         , maybe [] (\d -> ["--depth", T.pack $ show d]) depth
         , maybe [] (\h -> ["--branch", fromHash h]) hash'
         , [source, T.pack $ target]
         ])
    >> pure ()

checkout :: Ref -> EShell ()
checkout ref = git ["checkout", refName ref] >> pure ()

data ResetType = ResetHard | ResetSoft | ResetMixed
    deriving (Show, Eq, Ord)

instance CmdLineOption ResetType where
    optionToText ResetHard = "--hard"
    optionToText ResetSoft = "--soft"
    optionToText ResetMixed = "--mixed"

reset :: ResetType -> Ref -> EShell ()
reset rt ref = git ["reset", optionToText rt, refName ref] >> pure ()

data MergeFF = MergeFFOnly | MergeNoFF
      deriving (Show, Eq, Ord)

instance CmdLineOption MergeFF where
    optionToText MergeFFOnly = "--ff-only"
    optionToText MergeNoFF = "--no-ff"

merge :: MergeFF -> Branch -> EShell ()
merge mo branch = git ["merge", optionToText mo, branchFullName branch] >> pure ()

data PushType = PushNonForce | PushForceWithoutLease | PushForceWithLease

pushWith :: PushType -> [Text] -> EShell ()
pushWith pushType args =
    case pushType of
        PushNonForce -> push' []
        PushForceWithLease -> push' ["--force-with-lease"]
        PushForceWithoutLease -> push' ["--force"]
    where push' args' = git (["push"] ++ args' ++ args) >> pure ()

push :: EShell ()
push = pushWith PushNonForce []

pushForceWithLease :: EShell ()
pushForceWithLease = pushWith PushForceWithLease []

rebaseAbort :: EShell ()
rebaseAbort = git ["rebase", "--abort"] >> pure ()

remoteBranchPat :: Pattern (Remote, BranchName)
remoteBranchPat = do
    r <- Remote . nonEmptyText . T.pack <$> selfless (spaces *> some notSpace)
    name <- mkBranchName . T.pack <$> (char '/' *> some notSpace)
    eof
    return (r, name)

remoteBranches :: EShell [(Remote, BranchName)]
remoteBranches =
    (mapMaybe (singleMatch remoteBranchPat))
    <$> git ["branch", "--list", "-r", "--no-color"]

localBranchPat :: Pattern BranchName
localBranchPat = mkBranchName . T.pack <$> (spaces *> optional (char '*') *> spaces *> some notSpace) <* eof

localBranches :: EShell [BranchName]
localBranches = do
    allResults <- git ["branch", "--list", "--no-color"]
    return . catMaybes $ map (singleMatch localBranchPat) allResults

deleteLocalBranch :: BranchName -> EShell ()
deleteLocalBranch name = git ["branch", "-D", fromBranchName name] >> pure ()

deleteRemoteBranch :: Remote -> BranchName -> EShell ()
deleteRemoteBranch r n = do
    when (n == mkBranchName "master") $ abort "Refusing to delete remote master!"
    git ["push", "--delete", fromNonEmptyText $ remoteName r, fromBranchName n] >> pure ()

deleteBranch :: Branch -> EShell ()
deleteBranch branch =
    case branch of
        LocalBranch n -> deleteLocalBranch n
        RemoteBranch r n -> deleteRemoteBranch r n

createLocalBranch :: BranchName -> Ref -> EShell Branch
createLocalBranch name ref = do
    _ <- git ["checkout", "-b", fromBranchName name, refName ref]
    return $ LocalBranch name

pushRemoteTracking :: Remote -> BranchName -> PushType -> EShell Branch
pushRemoteTracking r name pushType = do
    pushWith pushType ["-u", fromNonEmptyText $ remoteName r, fromBranchName name]
    return $ RemoteBranch r name

exists :: Ref -> EShell Bool
exists ref = (git ["describe", "--always", refName ref] >> (return True))
    `catchError` (const $ return False)

data RebaseMergePolicy = RebaseKeepMerges | RebaseDropMerges

data Rebase =
    Rebase { rebaseBase   :: Ref
           , rebaseOnto   :: Ref
           , rebasePolicy :: RebaseMergePolicy
           }

rebase :: Rebase -> EShell ()
rebase (Rebase base onto policy) = git (["rebase", refName base, "--onto", refName onto] ++ p) >> pure ()
    where
        p = case policy of
            RebaseKeepMerges -> ["-p"]
            RebaseDropMerges -> []


currentRefHash :: EShell Hash
currentRefHash = hash . head <$> git ["log", "-1", "--format=%H"]

currentRef :: EShell Ref
currentRef = RefHash <$> currentRefHash

currentBranch :: EShell Branch
currentBranch = do
    res <- git ["symbolic-ref", "--short", "HEAD"]
    case safe head res of
        Just name -> return $ LocalBranch $ mkBranchName $ T.strip name
        Nothing -> abort $ "`git symbolic-ref --short HEAD` failed (note: git >= 1.8 required):\n" <> (T.intercalate "\n" res)

tempBranchNamePrefix :: Text
tempBranchNamePrefix = "sling-temp-"

genLocalTempBranchName :: EShell Text
genLocalTempBranchName = do
    existingTempBranches <-
        filter (T.isPrefixOf tempBranchNamePrefix)
        . map fromBranchName
        <$> localBranches
    let branchNumbersT :: [Text]
        branchNumbersT = map (T.drop (T.length tempBranchNamePrefix)) existingTempBranches
        branchNumbers :: [Int]
        branchNumbers = map (read . T.unpack) branchNumbersT
        highestNum :: Int
        highestNum = maybe 0 id $ safe maximum branchNumbers
    return $ tempBranchNamePrefix <> T.pack (show $ highestNum + 1)

withTempLocalBranch :: (BranchName -> EShell a) -> EShell a
withTempLocalBranch act = do
    newBranchName <- mkBranchName <$> genLocalTempBranchName
    mCurBranch <- (Just <$> currentBranch) `catchError` (\_ -> return Nothing)
    curRef <- currentRef
    tempBranch <- createLocalBranch newBranchName RefHead
    let cleanup = do
            -- Trying going back to previous branch, if there wasn't any or it was deleted just go to that ref hash
            case mCurBranch of
                Just curBranch -> checkout (RefBranch curBranch) `catchError` (\_ -> checkout curRef)
                Nothing -> checkout curRef
            deleteBranch tempBranch
    res <- act newBranchName `catchError` (\e -> cleanup >> throwError e)
    cleanup
    return res

isMergeCommit :: Ref -> EShell Bool
isMergeCommit ref = do
    headParents <- fmap T.words . safe head <$> git ["log", "-1", "--format=%p", refName ref, "--"]
    case length <$> headParents of
        Nothing -> abort "no git log output?!"
        Just 0 -> abort "commit has no parents?!" -- shouldn't happen
        Just 1 -> return False
        _ -> return True -- more than one parent

hashPat :: Pattern Hash
hashPat = hash . T.pack <$> some hexDigit

data LogEntry = LogEntry { logEntryFullHash  :: Hash
                         , logEntryShortHash :: Hash
                         , logEntryAuthor    :: Text
                         , logEntryTitle     :: Text }
    deriving (Show, Eq, Ord)

logPat :: Pattern LogEntry
logPat = do
    fullref <- hashPat <* spaces
    shortref <- hashPat <* spaces
    user <- T.pack <$> some (notChar '|')
    _ <- char '|'
    title <- T.pack <$> some anyChar
    return $ LogEntry fullref shortref user title


-- Run git log with trailing '--' to avoid ambiguity between branch and file names
log :: Ref -> Ref -> EShell [LogEntry]
log base top = join $ mapM (mustMatch logPat) <$> git ["log", "--format=%H %h %an|%s"
                                                      , refName base <> ".." <> refName top
                                                      , "--"]

getRef :: Ref -> EShell Ref
getRef ref = RefHash . hash . head <$> git ["log", "-1", "--format=%H", refName ref, "--"]

commitAmend :: Email -> Ref -> EShell ()
commitAmend email ref =
    git ["commit", "--amend", "-s", "--author=" <> formatEmail email, "-C", refName ref]
    >> pure ()

remoteUrl :: Remote -> EShell (Maybe Text)
remoteUrl r = safe head <$> git ["config", "remote." <> fromNonEmptyText (remoteName r) <> ".url"]

buildGithubCommitUrl :: Hash -> Text -> Text -> Text
buildGithubCommitUrl h user repo = "https://github.com/" <> user <> "/" <> repo <> "/commit/" <> fromHash h

githubUrlPat :: Pattern (Text, Text)
githubUrlPat = do
    user <- T.pack <$> (text "git@github.com:" *> some (notChar '/'))
    repo <- T.pack <$> some (notChar '.') <* text ".git"
    return (user, repo)

githubCommitUrl :: Hash -> Text -> (Maybe Text)
githubCommitUrl h url =
    uncurry (buildGithubCommitUrl h) <$> singleMatch githubUrlPat url
