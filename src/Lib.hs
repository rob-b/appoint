{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Lib where

import qualified GitHub.Data as GitHub
import qualified GitHub.Endpoints.PullRequests as GitHub
import qualified Data.Vector as V
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified GitHub.Auth as Auth
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid ((<>))
import GitHub.Request
import System.Environment (lookupEnv, getArgs)
import System.Exit (exitFailure)

getAuth :: IO (Maybe Auth.Auth)
getAuth = do
  mToken <- lookupEnv "GITHUB_TOKEN"
  return $ fmap fn mToken
    where fn t = Auth.OAuth (BS8.pack t)

getNameAndRepo :: IO (Maybe (T.Text, T.Text))
getNameAndRepo =
  getArgs >>=
  \case
    (name:repo:_) -> return $ Just (T.pack name, T.pack repo)
    _ -> return Nothing

usage :: String
usage = "Usage: appoint USERNAME|OWNERNAME REPO"

main :: IO ()
main = do
  auth <- getAuth
  auth' <-
    case auth of
      Nothing -> putStrLn "You must set GITHUB_TOKEN env var" >> exitFailure
      Just a -> return a

  pair <- getNameAndRepo
  pair' <- case pair of
      Nothing -> putStrLn usage >> exitFailure
      Just (a, b) -> return (GitHub.mkOwnerName a, GitHub.mkRepoName b)

  possiblePullRequests <- uncurry (pullRequestsFor auth') pair'
  case possiblePullRequests of
    (Left err) -> putStrLn ("Error: " ++ show err) >> exitFailure
    (Right pullRequests) -> T.putStrLn $ condense pullRequests

condense ::V.Vector GitHub.SimplePullRequest -> T.Text
condense = V.foldl' (\t pr -> formatPullRequest pr `T.snoc` '\n' <> t) ""

formatPullRequest :: GitHub.SimplePullRequest -> T.Text
formatPullRequest pullRequest =
  T.unlines $ filter (/= "")
    [ GitHub.simplePullRequestTitle pullRequest
    , maybe "" (T.take 50) (GitHub.simplePullRequestBody pullRequest)
    , displayUser $ GitHub.simplePullRequestUser pullRequest
    , T.pack . show $ V.map displayUser (GitHub.simplePullRequestAssignees pullRequest)
    , T.pack . show $ GitHub.simplePullRequestCreatedAt pullRequest
    , T.pack . show $ GitHub.simplePullRequestUpdatedAt pullRequest
    , GitHub.getUrl $ GitHub.simplePullRequestHtmlUrl pullRequest
    , ""]

displayUser :: GitHub.SimpleUser -> T.Text
displayUser = GitHub.untagName . GitHub.simpleUserLogin

pullRequestsFor
  :: GitHub.Auth
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> IO (Either GitHub.Error (V.Vector GitHub.SimplePullRequest))
pullRequestsFor auth user repo =
  executeRequest auth $ GitHub.pullRequestsForR user repo mempty GitHub.FetchAll
