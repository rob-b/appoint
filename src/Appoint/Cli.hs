{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings          #-}
module Appoint.Cli (main) where

import           Appoint.Assign            (listPrs)
import           Appoint.Entities          (doMigrations)
import           Appoint.Lib               (refresh)
import           Appoint.Types.Config      (RepoName(..), RepoOwner(..), mkAppState,
                                            mkPool, runAppT)
import qualified Data.ByteString.Char8     as BS8
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import           Database.Persist.Sql      (runSqlPool)
import qualified GitHub.Auth               as Auth
import           Options.Applicative
import           Options.Applicative.Types (readerAsk)
import           System.Environment        (lookupEnv)


data RepoIdentity =
  RepoIdentity RepoOwner
               RepoName
  deriving (Show)


data SubCommand
  = Refresh RepoIdentity
  | Assign RepoIdentity
   deriving (Show)


data Command = Command
  { cmdVerbose :: Bool
  , subCommand :: SubCommand
  } deriving (Show)


-------------------------------------------------------------------------------
argsWithInfo :: ParserInfo Command
argsWithInfo =
  info (helper <*> commandParser) (fullDesc <> progDesc "Pull request stuff")


-------------------------------------------------------------------------------
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


-------------------------------------------------------------------------------
verbosityParser :: Parser Bool
verbosityParser = switch (long "verbose" <> short 'v' <> help "Print debug output")


-------------------------------------------------------------------------------
commandParser :: Parser Command
commandParser = Command <$> verbosityParser <*> subs


-------------------------------------------------------------------------------
subs :: Parser SubCommand
subs =
  subparser $
  command "refresh" (withInfo refreshParser "refresh pull-request") <>
  command "assign" (withInfo assignParser "Assign PRs to a collaborator")


-------------------------------------------------------------------------------
refreshParser :: Parser SubCommand
refreshParser = Refresh <$> repoParser


-------------------------------------------------------------------------------
assignParser :: Parser SubCommand
assignParser = Assign <$> repoParser


-------------------------------------------------------------------------------
repoParser :: Parser RepoIdentity
repoParser =
  RepoIdentity <$> argument readerOwner (metavar "OWNER") <*>
  argument readerName (metavar "REPO")


-------------------------------------------------------------------------------
getAuth :: IO (Maybe Auth.Auth)
getAuth = do
  mToken <- lookupEnv "GITHUB_TOKEN"
  return $ fmap fn mToken
    where fn t = Auth.OAuth (BS8.pack t)


-------------------------------------------------------------------------------
readerOwner :: ReadM RepoOwner
readerOwner = RepoOwner . T.pack <$> readerAsk


-------------------------------------------------------------------------------
readerName :: ReadM RepoName
readerName = RepoName . T.pack <$> readerAsk


-------------------------------------------------------------------------------
main :: IO ()
main = do
  cmd <- execParser argsWithInfo
  auth <- getAuth
  pool <- mkPool (cmdVerbose cmd)
  runSqlPool doMigrations pool
  case subCommand cmd of
    Refresh (RepoIdentity owner' repo') -> do
      let state = mkAppState pool auth owner' repo'
      runAppT state refresh
    Assign (RepoIdentity owner' repo') -> do
      let state = mkAppState pool auth owner' repo'
      runAppT state listPrs
