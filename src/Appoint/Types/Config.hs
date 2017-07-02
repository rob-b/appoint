{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts      #-}
module Appoint.Types.Config where

import           Control.Lens
import           Control.Monad.Logger    (runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
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
  , _cOwner :: GitHub.Name GitHub.Owner
  , _cRepo :: GitHub.Name GitHub.Repo
  } deriving (Show)


makeLenses ''Config


newtype RepoName = RepoName
  { unRepoName :: Text
  } deriving (Show)

newtype RepoOwner = RepoOwner
  { unRepoOwner :: Text
  } deriving (Show)


-------------------------------------------------------------------------------
mkConfig :: Maybe Auth.Auth -> RepoOwner -> RepoName -> Config
mkConfig auth' owner name =
  Config
  { _cAuth = auth'
  , _cOwner = GitHub.mkOwnerName (unRepoOwner owner)
  , _cRepo = GitHub.mkRepoName (unRepoName name)
  }


-------------------------------------------------------------------------------
-- Create an sqlite db pool. Currently name and size are fixed
mkPool :: (MonadBaseControl IO m, MonadIO m) => Bool -> m ConnectionPool
mkPool verbose =
  if verbose
    then runStdoutLoggingT $ createSqlitePool "pr.sqlite" 1
    else runNoLoggingT $ createSqlitePool "pr.sqlite" 1


-------------------------------------------------------------------------------
mkAppState :: ConnectionPool -> Maybe GitHub.Auth -> RepoOwner -> RepoName -> AppState
mkAppState pool auth owner repo =
  let conf = mkConfig auth owner repo
  in AppState {appPool = pool, appConfig = conf, appLogger = print, appVerbose = False}


-------------------------------------------------------------------------------
runAppT :: AppState -> AppT m a -> m a
runAppT s m = runReaderT (unAppT m) s
