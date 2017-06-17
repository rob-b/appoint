module Appoint.Cli (main) where

import           Appoint.Assign (listPrs)
import           Appoint.Lib (refresh)
import           Appoint.Types.Config (mkConfig)
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified GitHub.Auth as Auth
import           Options.Applicative
import           Options.Applicative.Types (readerAsk)
import           System.Environment (lookupEnv)

data RefreshCommand =
  RefreshCommand
   deriving (Show)

data RepoIdentity =
  RepoIdentity T.Text
               T.Text
  deriving (Show)

data Command
  = Refresh RepoIdentity
  | Assign RepoIdentity
   deriving (Show)


-------------------------------------------------------------------------------
argsWithInfo :: ParserInfo Command
argsWithInfo =
  info (helper <*> commandParser) (fullDesc <> progDesc "Pull request stuff")


-------------------------------------------------------------------------------
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


-------------------------------------------------------------------------------
commandParser :: Parser Command
commandParser =
  subparser $
  command "refresh" (withInfo refreshParser "refresh pull-request") <>
  command "assign" (withInfo assignParser "Assign PRs to a collaborator")


-------------------------------------------------------------------------------
refreshParser :: Parser Command
refreshParser = Refresh <$> repoParser


-------------------------------------------------------------------------------
assignParser :: Parser Command
assignParser = Assign <$> repoParser


-------------------------------------------------------------------------------
repoParser :: Parser RepoIdentity
repoParser =
  RepoIdentity <$> argument readerText (metavar "OWNER") <*>
  argument readerText (metavar "REPO")


-------------------------------------------------------------------------------
getAuth :: IO (Maybe Auth.Auth)
getAuth = do
  mToken <- lookupEnv "GITHUB_TOKEN"
  return $ fmap fn mToken
    where fn t = Auth.OAuth (BS8.pack t)


-------------------------------------------------------------------------------
readerText :: ReadM T.Text
readerText = T.pack <$> readerAsk


-------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- execParser argsWithInfo
  case args of
    Refresh (RepoIdentity owner' repo') -> do
      auth <- getAuth
      let config = mkConfig auth owner' repo'
      refresh config
    Assign (RepoIdentity owner' repo') -> do
      auth <- getAuth
      let config = mkConfig auth owner' repo'
      listPrs config
