{-# LANGUAGE TemplateHaskell #-}
module Appoint.Types.Config where
import qualified GitHub.Data as GitHub
import qualified GitHub.Auth as Auth
import Data.Text (Text)
import Control.Lens

data Config = Config
  { _cAuth :: Maybe Auth.Auth
  , _cName :: GitHub.Name GitHub.Owner
  , _cRepo :: GitHub.Name GitHub.Repo
  , _cVerbose :: Bool
  } deriving (Show)

makeLenses ''Config

mkConfig :: Maybe Auth.Auth -> Text -> Text -> Config
mkConfig auth' name' repo' =
  Config
  { _cAuth = auth'
  , _cName = GitHub.mkOwnerName name'
  , _cRepo = GitHub.mkRepoName repo'
  , _cVerbose = True
  }
