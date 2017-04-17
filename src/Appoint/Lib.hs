{-# LANGUAGE OverloadedStrings #-}
module Appoint.Lib where

import           Appoint.Models          (saveIssues)
import qualified Data.Vector             as V
import qualified GitHub.Auth             as Auth
import qualified GitHub.Data             as GitHub
import qualified GitHub.Endpoints.Search as GitHub


-------------------------------------------------------------------------------
-- | Search for PRs matching a hardcoded label, save results to db
refresh :: Maybe Auth.Auth -> IO ()
refresh auth = searchPrs auth >>= saveIssues


-------------------------------------------------------------------------------
searchPrs :: Maybe Auth.Auth -> IO (V.Vector GitHub.Issue)
searchPrs auth = do
  things <-
    GitHub.searchIssues' auth "repo:somerepo label:\"project: example-label\""
  case things of
    Left err' -> error (show err')
    Right results -> return $ GitHub.searchResultResults results
