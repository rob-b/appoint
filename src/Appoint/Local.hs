{-# LANGUAGE OverloadedStrings #-}
module Appoint.Local where

import Appoint.Types.Users (Collaborators(..))
import Control.Exception (try, IOException)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C

safeReadFile :: FilePath -> IO (Either IOException C.ByteString)
safeReadFile = try . C.readFile

collaboratorsFile :: IO (Either IOException C.ByteString)
collaboratorsFile = safeReadFile ".collaborators.json"

-- | Something went wrong either while reading the local file or updating its
-- | contents from its remote source
data SomethingBad
  = SomethingRemote String
  | SomethingJson JsonLoadError
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

saveCollaboratorsToFile :: ToJSON a => a -> IO ()
saveCollaboratorsToFile users =
  C.writeFile ".collaborators.json" (encode users) >> putStrLn "ok"
