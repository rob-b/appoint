{-# LANGUAGE OverloadedStrings #-}
module Appoint.Local where

import Appoint.Types.Users (Collaborators(..))
import qualified Appoint.Types.Users as U
import Control.Exception (try, IOException)
import Control.Lens ((^.))
import Data.Aeson
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.IO.Error
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as Map
import qualified Data.Text as T

safeReadFile :: FilePath -> IO (Either IOException C.ByteString)
safeReadFile = try . C.readFile

collaboratorsFile :: IO (Either IOException C.ByteString)
collaboratorsFile = safeReadFile ".collaborators.json"

-- | Something went wrong either while reading the local file or updating its
-- | contents from its remote source
data SomethingBad
  = SomethingRemote String
  | SomethingJson JsonLoadError
  | SomethingMissing T.Text
   deriving (Show)

-- | When something goes wrong while reading + parsing the Json file
data JsonLoadError
  = JsonParseError String
  | JsonIOError IOException
   deriving (Show)

collaboratorsFromFile :: IO (Either JsonLoadError [Collaborators])
collaboratorsFromFile = do
  collabs <- collaboratorsFile
  case collabs of
    Right result ->
      return $ either (Left . JsonParseError) Right (eitherDecode result)
    Left err -> return $ Left (JsonIOError err)

collabsMap :: [Collaborators] -> Map.Map T.Text [U.User]
collabsMap = foldl' (\m c -> Map.insert (c ^. U.path) (c ^. U.users) m) Map.empty

saveCollaboratorsToFile :: ToJSON a => a -> IO ()
saveCollaboratorsToFile users =
  C.writeFile ".collaborators.json" (encode users) >> putStrLn "ok"

describeJsonError :: JsonLoadError -> T.Text
describeJsonError (JsonParseError msg) =
  T.pack $ "Error while parsing json: " <> msg
describeJsonError (JsonIOError exc) =
  T.pack $
  "Error while opening " <> fromMaybe "" (ioeGetFileName exc) <> ": " <>
  ioeGetErrorString exc
