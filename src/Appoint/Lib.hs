{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Appoint.Lib where

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
import Appoint.Wrap

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
  let handler = maybe GitHub.pullRequestsFor pullRequestsFor auth
  pair <- getNameAndRepo
  pair' <-
    case pair of
      Nothing -> putStrLn usage >> exitFailure
      Just (a,b) -> return (GitHub.mkOwnerName a, GitHub.mkRepoName b)
  possiblePullRequests <- uncurry handler pair'
  case possiblePullRequests of
    Left err -> putStrLn (mkErrorMsg auth err) >> exitFailure
    Right pullRequests -> T.putStrLn $ condense pullRequests

mkErrorMsg :: Show a => Maybe Auth.Auth -> a -> String
mkErrorMsg Nothing _ =
  "Error while making unauthenticated request. " <>
  "Perhaps this repo is private in which case you must set the GITHUB_TOKEN env var."
mkErrorMsg (Just _) err = show err

condense ::V.Vector GitHub.SimplePullRequest -> T.Text
condense = V.foldl' (\t pr -> formatPullRequest pr `T.snoc` '\n' <> t) ""

formatPullRequest :: GitHub.SimplePullRequest -> T.Text
formatPullRequest pullRequest =
  T.unlines $ filter (/= "")
    [ GitHub.simplePullRequestTitle pullRequest
    , T.replicate (T.length $ GitHub.simplePullRequestTitle pullRequest) "="
    , maybe "" (wrapParagraph 72) (GitHub.simplePullRequestBody pullRequest)
    , "Author: " <> displayUser (GitHub.simplePullRequestUser pullRequest)
    , assignedTo (GitHub.simplePullRequestAssignees pullRequest)
    -- , T.pack . show $ GitHub.simplePullRequestCreatedAt pullRequest
    -- , T.pack . show $ GitHub.simplePullRequestUpdatedAt pullRequest
    , GitHub.getUrl $ GitHub.simplePullRequestHtmlUrl pullRequest]

assignedTo :: V.Vector GitHub.SimpleUser -> T.Text
assignedTo as
  | not (V.null as) = "Assigned to: " <> displayAssignees as
  | otherwise = ""

displayAssignees :: V.Vector GitHub.SimpleUser -> T.Text
displayAssignees as
  | not (V.null as) = T.intercalate ", " (V.toList (V.map displayUser as))
  | otherwise = ""

displayUser :: GitHub.SimpleUser -> T.Text
displayUser = GitHub.untagName . GitHub.simpleUserLogin

pullRequestsFor
  :: GitHub.Auth
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> IO (Either GitHub.Error (V.Vector GitHub.SimplePullRequest))
pullRequestsFor auth user repo =
  executeRequest auth $ GitHub.pullRequestsForR user repo mempty GitHub.FetchAll
