{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Appoint.Types.Config where

import           Control.Lens
import           Control.Monad.Logger    (runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Reader
import           Data.Text               (Text)
import           Database.Persist.Sql    (ConnectionPool)
import           Database.Persist.Sqlite (createSqlitePool)
import qualified GitHub.Auth             as Auth
import qualified GitHub.Data             as GitHub


data AppState = AppState
  { appConfig :: Config
  , appPool :: ConnectionPool
  , appVerbose :: Bool
  , appLogger :: String -> IO ()
  }


newtype AppT m a = AppT
  { unAppT :: ReaderT AppState m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState)


data Config = Config
  { _cAuth :: Maybe Auth.Auth
  , _cName :: GitHub.Name GitHub.Owner
  , _cRepo :: GitHub.Name GitHub.Repo
  } deriving (Show)


makeLenses ''Config


-------------------------------------------------------------------------------
mkConfig :: Maybe Auth.Auth -> Text -> Text -> Config
mkConfig auth' name' repo' =
  Config
  { _cAuth = auth'
  , _cName = GitHub.mkOwnerName name'
  , _cRepo = GitHub.mkRepoName repo'
  }


-------------------------------------------------------------------------------
-- Create an sqlite db pool. Currently name and size are fixed
mkPool :: Bool -> IO ConnectionPool
mkPool verbose =
  if verbose
    then runStdoutLoggingT $ createSqlitePool "pr.sqlite" 1
    else runNoLoggingT $ createSqlitePool "pr.sqlite" 1


-------------------------------------------------------------------------------
mkAppState :: ConnectionPool -> Maybe GitHub.Auth -> Text -> Text -> AppState
mkAppState pool auth name repo =
  let conf = mkConfig auth name repo
  in AppState {appPool = pool, appConfig = conf, appLogger = print, appVerbose = False}


-------------------------------------------------------------------------------
runAppT :: AppState -> AppT m a -> m a
runAppT s m = runReaderT (unAppT m) s
