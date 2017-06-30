{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Appoint.Lib where

import           Appoint.Models          (balanceIssues)
import           Appoint.Types.Config
import           Control.Lens
import           Control.Monad.Except    (ExceptT, liftIO, runExceptT,
                                          throwError)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Reader    (MonadReader, asks)
import           Data.Monoid             ((<>))
import qualified Data.Vector             as V
import qualified GitHub.Data             as GitHub
import qualified GitHub.Endpoints.Search as GitHub
import           System.Exit             (exitFailure)


-------------------------------------------------------------------------------
-- | Search for PRs, save results to db
refresh
  :: (MonadIO m, MonadReader AppState m)
  => m ()
refresh = do
  config <- asks appConfig
  result <- runExceptT (searchPrs config)
  case result of
    Left _
     -> liftIO exitFailure
    Right issues -> balanceIssues issues


newtype SearchError =
  SearchError String
  deriving (Show)


-------------------------------------------------------------------------------
searchPrs
  :: (MonadIO m)
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
      pure results'
