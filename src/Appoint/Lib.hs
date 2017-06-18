{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Appoint.Lib where

import           Appoint.Models          (saveIssues)
import           Appoint.Types.Config
import           Control.Lens
import           Data.Monoid             ((<>))
import qualified Data.Vector             as V
import qualified GitHub.Data             as GitHub
import qualified GitHub.Endpoints.Search as GitHub
import Control.Monad.Except (ExceptT, throwError, liftIO, runExceptT)
import System.Exit (exitFailure)
import Control.Monad.Log
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T


-------------------------------------------------------------------------------
-- | Search for PRs, save results to db
refresh :: (MonadIO m, MonadLog (WithSeverity Text) m) => Config -> m ()
refresh config = do
  result <- runExceptT (searchPrs config)
  case result of
    Left yikes -> do
      logMessage $ info (T.pack ("Failed to refresh: " <> show yikes))
      liftIO exitFailure
    Right issues -> liftIO $ saveIssues issues


info :: a -> WithSeverity a
info = WithSeverity Informational


newtype SearchError =
  SearchError String
  deriving (Show)


-------------------------------------------------------------------------------
searchPrs
  :: (MonadIO m, MonadLog (WithSeverity Text) m)
  => Config -> ExceptT SearchError m (V.Vector GitHub.Issue)
searchPrs config = do
  let owner' = GitHub.untagName $ config ^. cName
      repo' = GitHub.untagName $ config ^. cRepo
      auth = config ^. cAuth
  things <-
    liftIO $ GitHub.searchIssues' auth ("is:pr is:open repo:" <> owner' <> "/" <> repo')
  case things of
    Left err' -> throwError $ SearchError (show err')
    Right results -> do
      let results' = GitHub.searchResultResults results
      logMessage $ info (T.pack $ "Found " <> show (V.length results') <> " PR(s)")
      pure results'
