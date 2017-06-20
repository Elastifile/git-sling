{-# LANGUAGE OverloadedStrings #-}
module Sling.Options
    where

import Sling.Proposal (Prefix, prefixFromText, Proposal(..), parseProposal)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Control.Applicative ((<|>))
import           Options.Applicative (Mod, Parser, OptionFields,
                                      argument, auto, command, execParser, flag, flag', fullDesc,
                                      header, help, helper, hsubparser,
                                      info, long, metavar, progDesc, option, optional, short,
                                      some, str, strOption, switch, value)

data PollMode = PollModeDaemon Int | PollModeOneShot | PollModeAllQueued

data PollOptions =
    PollOptions
    { optBranchFilterAll :: Maybe String
    , optBranchExcludeFilterAll :: Maybe String
    , optBranchFilterDryRun :: Maybe String
    , optBranchFilterNoDryRun :: Maybe String
    , optSourcePrefix :: Maybe Prefix
    , optPollMode :: PollMode
    , optNoConcurrent :: Bool
    , optInProgressFromAnyServer :: Bool
    }


data OptServerId = OptServerIdHostName | OptServerIdName Text

data Options =
    Options
    { optWebServerPort :: Int
    , optEmailClient :: [Text]
    , optForceDryRun :: Bool
    , optTargetPrefix :: Maybe Prefix
    , optServerId :: OptServerId
    , optCommandType :: CommandType
    }

data PrepushCmd = PrepushCmd [String]

data CommandType
    = CommandTypePropose { _cmdMode :: PrepushMode
                         , _cmdPrepushCommandAnArgs :: PrepushCmd }
    | CommandTypeList PollOptions

data PrepushMode
    = ProposalModePoll PollOptions
    | ProposalModeSingle Proposal

parseOpts :: IO Options
parseOpts = execParser $
    info (helper <*> parser)
    (fullDesc <> header "git-sling - merge branches with due process")

defaultEmailClient :: [Text]
defaultEmailClient = ["msmtp", "-C", "/opt/msmtp.conf"]

defaultPort :: Int
defaultPort = 8080

catchMaybe :: Maybe c -> c -> c
catchMaybe = flip fromMaybe

parseProposalFromCmdLine :: String -> Proposal
parseProposalFromCmdLine s =
    parseProposal (T.pack s)
    `catchMaybe` error ("Invalid proposal format: " ++ show s)

prepushCommandArgs :: Parser PrepushCmd
prepushCommandArgs = PrepushCmd <$>
    some (argument str
              (metavar "-- PREPUSH COMMAND LINE..." <>
               help "Pre-push command to run on each proposed branch (exit code 0 considered success)"))

parseModeBranches :: Parser CommandType
parseModeBranches =
    hsubparser
    ( command "proposal" (info
                          (CommandTypePropose <$>
                           (ProposalModeSingle .parseProposalFromCmdLine <$>
                               argument str (metavar "PROPOSAL" <>
                                             help "A proposal branch name to handle"))
                           <*> prepushCommandArgs)
                          (fullDesc <> progDesc "Process a single proposal"))
      <> command "list" (info (CommandTypeList <$> pollOptionsParser)
                         (fullDesc <> progDesc "List pending proposals"))
      <> command "poll" (info (CommandTypePropose
                                  <$> (ProposalModePoll <$> pollOptionsParser)
                                  <*> prepushCommandArgs)
                         (fullDesc <> progDesc "Process proposals queued as branches"))
    )
    -- flag' (ProposalFromBranch Nothing)
    --  (short 's' <>
    --   long "single" <>
    --   help "Process proposals from current (no polling) branches: Fetch current branches and process all existing proposals, then exit")
    -- <|> (ProposalFromBranch <$>
    --         optional (option auto
    --             (short 'd' <>
    --              metavar "T" <>
    --              long "daemon" <>
    --              help )))
    -- <|> (ProposalFromCommandLine . parseProposalFromCmdLine <$>
    --      strOption (long "proposal-branch" <>
    --                 short 'b' <>
    --                 help "A proposal branch name to handle"))

prefixOption :: Mod OptionFields String -> Parser Prefix
prefixOption args = prefixFromText . verify . T.pack <$> strOption args
    where
        verify t = if T.null t then error "Prefix can't be empty" else t

pollOptionsParser :: Parser PollOptions
pollOptionsParser =
    PollOptions
    <$> optional (strOption
                  (long "match-branches" <>
                   metavar "PATTERN" <>
                   help "Regex pattern to match 'onto' branch name in any proposal"))
    <*> optional (strOption
                  (long "exclude-branches" <>
                   metavar "PATTERN" <>
                   help "Regex pattern to exclude 'onto' branch name in any proposal"))
    <*> optional (strOption
                  (long "match-dry-run-branches" <>
                   metavar "PATTERN" <>
                   help "Regex pattern to match 'onto' branch name in dry run proposals"))
    <*> optional (strOption
                  (long "match-non-dry-run-branches" <>
                   metavar "PATTERN" <>
                   help "Regex pattern to match 'onto' branch name in non-dry run proposals"))
    <*> optional (prefixOption
                  (long "source-prefix" <>
                   help "Exact prefix of branches to be used for proposals (other proposals will be ignored)"))
    <*> (PollModeDaemon <$> (option auto $
                             short 'd' <>
                             metavar "T" <>
                             long "daemonize" <>
                             help "'Daemonize' - run endlessly, polling proposals from branches every T seconds")
         <|> (flag' PollModeOneShot
              (long "one-shot" <>
               help "Process one proposal and then quit; if nothing to process, quit immediately"))
         <|> (flag PollModeAllQueued PollModeAllQueued
              (long "all" <>
               help "(Default) Process all pending proposals and then quit; if nothing to process, quit immediately"))
        )
    <*> switch ( long "no-concurrent" <>
                  help "Prevent concurrent jobs: don't match any proposal, if there's an in-progress proposal matching the filter" )
    <*> switch ( long "any-host" <>
                  help "Match in-progress proposals even if they belong to a different host" )


parser :: Parser Options
parser =
    Options
    <$> (option auto
          (value defaultPort <>
           short 'p' <>
           long "port" <>
           metavar "PORT" <>
           help ("Port for sling web server. Default: " <> show defaultPort)))
    <*> (fmap (maybe defaultEmailClient (T.words . T.pack)) <$>
         optional $ strOption
         (short 'e' <>
          long "email-client" <>
          metavar "EMAIL_CLIENT_COMMAND" <>
          help ("Command to use sending emails. Default: " <> T.unpack (T.intercalate " " defaultEmailClient))))
    <*> switch (short 'n' <>
                long "force-dry-run" <>
                help "Treat all proposals as dry run (regardless of what they say)")
    <*> optional (prefixOption
                  (long "target-prefix" <>
                   help "ITypef missing, successful branches are merged (unless dry-run) & deleted. If non-empty, prefix of branches to be used for succesful proposals (branches will not be merged)"))
    <*> (fmap (maybe OptServerIdHostName (OptServerIdName . T.pack)) <$>
         optional $ strOption
         (long "server-id" <>
          metavar "SERVER_ID" <>
          help ("Used for resuming in-progress jobs that were aborted due to a failed server")))
    <*> parseModeBranches

isDryRun :: Options -> Proposal -> Bool
isDryRun options proposal = optForceDryRun options || proposalDryRun proposal


