{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}
module Appoint.Lib where

import qualified GitHub.Data as GitHub
import qualified GitHub.Endpoints.PullRequests as GitHub
import qualified Data.Vector as V
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified GitHub.Auth as Auth
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Appoint.Wrap
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import GitHub.Data.Definitions (Error)
import GitHub.Request
import Rainbow
import System.Environment (lookupEnv, getArgs)
import System.Exit (exitFailure)

data OutputKind = Plain | Colour

data Output = Output
  { oTitle :: T.Text
  , oBody :: T.Text
  , oAuthor :: T.Text
  , oAssignees :: T.Text
  , oAssigneeCount :: Int
  , oCreated :: UTCTime
  , oURL :: T.Text
  } deriving (Show, Eq)

main :: IO ()
main = do
  auth <- getAuth
  let handler = maybe GitHub.pullRequestsFor pullRequestsFor auth
  pair <- getNameAndRepo
  pair' <-
    case pair of
      Nothing -> putStrLn usage >> exitFailure
      Just (a,b) -> return (GitHub.mkOwnerName a, GitHub.mkRepoName b)
  prs <- doRequest handler pair' auth
  -- printTitlesForSelection prs
  printPRs Colour prs

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

printTitlesForSelection :: V.Vector Output -> IO ()
printTitlesForSelection prs = T.putStrLn $ T.intercalate "\n" (selectUnassigned (V.toList prs))
  where
    selectUnassigned :: [Output] -> [T.Text]
    selectUnassigned prs' = [uncurry imapfn (ix, pr) | pr <- prs', hasAssignees pr | ix <- (map succ [0..])]
    hasAssignees :: Output -> Bool
    hasAssignees pr = oAssigneeCount pr == 0
    imapfn :: Integer -> Output -> T.Text
    imapfn ix pr = redic ix <> " -> " <> oTitle pr

    redic :: Integer -> T.Text
    redic i = T.pack(show i)

printPRs :: OutputKind -> V.Vector Output -> IO ()
printPRs Colour prs = do
  let colourisedPrs = map colourOutput (V.toList prs)
  mapM_ printChunks colourisedPrs
printPRs Plain prs = do
  let plainPrs = map plainOutput (V.toList prs)
  mapM_ T.putStrLn plainPrs

plainOutput :: Output -> T.Text
plainOutput o =
  foldl' (\a b -> a <> b <> "\n") "" [ x | x <- fields, x /= ""]
  where
    fields = [ oTitle o , T.replicate (T.length $ oTitle o) "=", oAuthor o , oAssignees o , oBody o]

colourOutput :: Output -> [Maybe (Chunk T.Text)]
colourOutput o =
  [ Just $ chunk (oTitle o) <> chunk "\n" & fore green
  , Just $ chunk (oAuthor o) <> chunk "\n" & fore yellow
  , assigneesChunk
  , Just $ chunk (oBody o) <> chunk "\n"]
  where
    assigneesChunk =
      if T.null (oAssignees o)
        then Nothing
        else Just $ chunk (oAssignees o) <> chunk "\n" & fore brightYellow

printChunks :: Renderable a => [Maybe (Chunk a)] -> IO ()
printChunks cs =
  mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256 $ catMaybes cs

type Handler a b = (a -> b -> IO (Either Error (V.Vector GitHub.SimplePullRequest)))

doRequest
  :: Handler a b
  -> (a, b)
  -> Maybe Auth.Auth
  -> IO (V.Vector Output)
doRequest handler p auth = do
  possiblePullRequests <- uncurry handler p
  case possiblePullRequests of
    Left err -> putStrLn (mkErrorMsg auth err) >> exitFailure
    Right pullRequests -> return $ f pullRequests
    where f = V.map toOutput

mkErrorMsg :: Show a => Maybe Auth.Auth -> a -> String
mkErrorMsg Nothing _ =
  "Error while making unauthenticated request. " <>
  "Perhaps this repo is private in which case you must set the GITHUB_TOKEN env var."
mkErrorMsg (Just _) err = show err

toOutput :: GitHub.SimplePullRequest -> Output
toOutput pullRequest =
  Output
  { oTitle = GitHub.simplePullRequestTitle pullRequest
  , oBody = maybe "" (wrapParagraph 72) (GitHub.simplePullRequestBody pullRequest)
  , oAuthor = "Author: " <> displayUser (GitHub.simplePullRequestUser pullRequest)
  , oAssignees = assignedTo (GitHub.simplePullRequestAssignees pullRequest)
  , oAssigneeCount = V.length (GitHub.simplePullRequestAssignees pullRequest)
  , oCreated = GitHub.simplePullRequestCreatedAt pullRequest
  , oURL = GitHub.getUrl $ GitHub.simplePullRequestHtmlUrl pullRequest
  }

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
  executeRequest auth $ GitHub.pullRequestsForR user repo mempty (GitHub.FetchAtLeast 20)
