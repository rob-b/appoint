module Appoint.Cli (main) where

import           Appoint.Assign            (listPrs, mkOwnershipParams)
import           Appoint.Lib               (refresh)
import qualified Data.ByteString.Char8     as BS8
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import qualified GitHub.Auth               as Auth
import           Options.Applicative
import           Options.Applicative.Types (readerAsk)
import           System.Environment        (lookupEnv)


data RefreshCommand =
  RefreshCommand
   deriving (Show)

data AssignCommand = AssignCommand
  { assignOwner :: T.Text
  , assignRepo :: T.Text
  } deriving (Show)

data Command
  = Refresh
  | Assign AssignCommand
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
refreshParser = pure Refresh


-------------------------------------------------------------------------------
assignParser :: Parser Command
assignParser = Assign <$> assignCommandParser


assignCommandParser :: Parser AssignCommand
assignCommandParser =
  AssignCommand <$> argument readerText (metavar "OWNER") <*>
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
    Refresh -> getAuth >>= refresh
    Assign (AssignCommand owner' repo') -> do
      auth <- getAuth
      uncurry listPrs (mkOwnershipParams owner' repo') auth
