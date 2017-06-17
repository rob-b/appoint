{-# LANGUAGE OverloadedStrings #-}
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


-------------------------------------------------------------------------------
-- | Search for PRs, save results to db
refresh :: Config -> IO ()
refresh config = do
  result <- runExceptT (searchPrs config)
  case result of
    Left yikes -> putStrLn ("Failed to refresh: " <> show yikes) >> exitFailure
    Right issues -> saveIssues issues


newtype AppointError =
  AppointError String
  deriving (Show)

-------------------------------------------------------------------------------
searchPrs :: Config -> ExceptT AppointError IO (V.Vector GitHub.Issue)
searchPrs config = do
  let owner' = GitHub.untagName $ config ^. cName
      repo' = GitHub.untagName $ config ^. cRepo
      auth = config ^. cAuth
  things <-
    liftIO $ GitHub.searchIssues' auth ("is:pr is:open repo:" <> owner' <> "/" <> repo')
  case things of
    Left err' -> throwError $ AppointError (show err')
    Right results -> do
      let results' = GitHub.searchResultResults results
      if config ^. cVerbose
        then liftIO . putStrLn $ "Found " <> show (V.length results') <> " PR(s)"
        else pure ()
      pure results'
