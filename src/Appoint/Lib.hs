{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}
module Appoint.Lib where

import qualified GitHub.Data as GitHub
import qualified GitHub.Data.Issues as GitHub
import qualified GitHub.Endpoints.PullRequests as GitHub
import qualified GitHub.Endpoints.Search as GitHub
import qualified Data.Vector as V
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified GitHub.Auth as Auth
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString as BS
import Appoint.Wrap
import Control.Lens ((^.))
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.List (foldl')
import Data.Aeson
import Control.Monad (forM_, forM, mapM_)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import GitHub.Data.Definitions (Error)
import GitHub.Request
import Rainbow
import System.Environment (lookupEnv, getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

import Appoint.Users (collaboratorsOn, mkCollaborators)
import Appoint.Types.Users (User, Collaborators(..))
import Appoint.Types.Issues
import qualified Appoint.Types.Users as U
import Appoint.Types.Config
import Appoint.Local
       (saveCollaboratorsToFile, collaboratorsFromFile, SomethingBad(..),
        JsonLoadError(..), describeJsonError)
import Appoint.Models (persistLabel, persistIssue, persistIssueLabel)

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
  pair <- getNameAndRepo
  pair' <-
    case pair of
      Nothing -> putStrLn usage >> exitFailure
      Just (a,b) -> return (GitHub.mkOwnerName a, GitHub.mkRepoName b)
  -- uncurry listPrs pair' auth
  results <- searchPrs auth
  saveIssues (V.take 10 results)
  forM_ (V.take 1 results) (BSL8.putStrLn . encode)
  -- let config = uncurry (mkConfig auth) pair'
  -- collabs <- runReaderT collaboratorsOn config
  -- case collabs of
  --   Left _ -> error "no such luck"
  --   Right collabs' -> saveCollaboratorsToFile $ uncurry mkCollaborators pair' (V.toList collabs')


saveIssues :: Foldable t => t GitHub.Issue -> IO ()
saveIssues issues = forM_ issues $ \issue -> do
  labelIds <- saveLabelsFromIssue issue
  issueId' <- persistIssue issue
  persistIssueLabel issueId' labelIds


-- saveLabelsFromIssue :: GitHub.Issue -> IO ()
saveLabelsFromIssue issue = forM (GitHub.issueLabels issue) persistLabel


searchPrs :: Maybe Auth.Auth -> IO (V.Vector GitHub.Issue)
searchPrs auth = do
  things <- GitHub.searchIssues' auth "repo:somerepo label:\"project: example-label\""
  case things of
    Left _ -> error "Not interested in this right now"
    Right results -> return $ GitHub.searchResultResults results


listPrs :: GitHub.Name GitHub.Owner -> GitHub.Name GitHub.Repo -> Maybe Auth.Auth -> IO ()
listPrs owner repo auth = do
  let handler = maybe GitHub.pullRequestsFor pullRequestsFor auth
  prs <- doRequest handler owner repo auth
  -- interactiveAppoint prs config
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

-------------------------------------------------------------------------------
--- Interactive selection of PR and reviewer
-------------------------------------------------------------------------------

interactiveAppoint :: V.Vector Output -> Config -> IO ()
interactiveAppoint prs config = do
  printTitlesForSelection prs
  selection <- selectPR (V.length prs)
  action <- showCollaborators config
  case action of
    Proceed collaborators -> do
      let collab = filterCollaborators "somerepo" collaborators
      case collab of
        Nothing -> T.putStrLn "Lukewarm" >> exitFailure
        Just c ->
          printUsersForSelection (c ^. U.users) >>
          selectUser (length collaborators) >>=
          print >>
          exitFailure
    Halt msg -> T.putStrLn msg >> exitFailure
    Retry -> T.putStrLn "Cannot proceed without collaborators" >> exitFailure
  print selection

filterCollaborators :: T.Text -> [Collaborators] -> Maybe Collaborators
filterCollaborators _ [] = Nothing
filterCollaborators target cs = listToMaybe $ filter (\c -> c ^. U.path == target) cs

selectUser :: (Read b, Num b, Eq b, Enum b) => b -> IO b
selectUser max' = do
  T.putStrLn "Select User"
  choice <- getLine
  let choice' = readMaybe choice
  case choice' of
    Nothing -> T.putStrLn "Invalid selection" >> selectUser max'
    Just x -> if x `elem` [1..max'] then return x else selectUser max'

selectPR :: (Read b, Num b, Eq b, Enum b) => b -> IO b
selectPR max' = do
  T.putStrLn "Select PR"
  choice <- getLine
  let choice' = readMaybe choice
  case choice' of
    Nothing -> T.putStrLn "Invalid selection" >> selectPR max'
    Just x -> if x `elem` [1..max'] then return x else selectPR max'

showCollaborators :: Config -> IO Action
showCollaborators config = do
  collabs <- runExceptT $ runReaderT getCollaborators config
  return $ convertToAction collabs

data Action = Proceed [Collaborators] | Halt T.Text | Retry

convertToAction :: Either SomethingBad [Collaborators] -> Action
convertToAction (Left (SomethingRemote _)) = error "We have not encountered this code path yet"
convertToAction (Left (SomethingMissing namerepo)) = error(show namerepo)
convertToAction (Left (SomethingJson msg)) = Halt $ describeJsonError msg
convertToAction (Right collaborators) = Proceed collaborators

getCollaborators :: ReaderT Appoint.Types.Config.Config (ExceptT SomethingBad IO) [Collaborators]
getCollaborators =
  lift $ do
    collabs <- lift collaboratorsFromFile
    case collabs of
      Right collaborators -> return collaborators
      Left (JsonParseError e) -> throwE $ SomethingJson (JsonParseError e)
      Left (JsonIOError ioerror) -> throwE $ SomethingJson (JsonIOError ioerror)

usage :: String
usage = "Usage: appoint USERNAME|OWNERNAME REPO"

printXForSelection :: (Integer -> a -> T.Text) -> [a] -> IO ()
printXForSelection fn xs = T.putStrLn . T.intercalate "\n" $ indexed fn xs
  where
    indexed :: (Integer -> a -> T.Text) -> [a] -> [T.Text]
    indexed fn' xs' = [uncurry fn' (i, x) | x <- xs' | i <- (fmap succ [0..])]

printUsersForSelection :: [User] -> IO ()
printUsersForSelection = printXForSelection fmt
    where
      fmt :: Integer -> User -> T.Text
      fmt ix u = T.pack (show ix) <> " -> " <> U.userName u

printTitlesForSelection :: V.Vector Output -> IO ()
printTitlesForSelection xs = printXForSelection fmt (V.toList xs)
  where
    fmt :: Integer -> Output -> T.Text
    fmt ix pr = T.pack (show ix) <> " -> " <> oTitle pr

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
  -> a
  -> b
  -> Maybe Auth.Auth
  -> IO (V.Vector Output)
doRequest handler owner repo auth = do
  possiblePullRequests <- handler owner repo
  case possiblePullRequests of
    Left err -> putStrLn (mkRequestErrorMsg auth err) >> exitFailure
    Right pullRequests -> return $ f pullRequests
    where f = V.map toOutput

mkRequestErrorMsg :: Show a => Maybe Auth.Auth -> a -> String
mkRequestErrorMsg Nothing _ =
  "Error while making unauthenticated request. " <>
  "Perhaps this repo is private in which case you must set the GITHUB_TOKEN env var."
mkRequestErrorMsg (Just _) err = show err

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
