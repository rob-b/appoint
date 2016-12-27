{-# LANGUAGE TemplateHaskell #-}
module Appoint.Types.Config where
import qualified GitHub.Data as GitHub
import qualified GitHub.Auth as Auth
import Control.Lens

data Config = Config
  { _cAuth :: Maybe Auth.Auth
  , _cName :: GitHub.Name GitHub.Owner
  , _cRepo :: GitHub.Name GitHub.Repo
  } deriving (Show)

makeLenses ''Config

mkConfig :: Maybe Auth.Auth -> GitHub.Name GitHub.Owner -> GitHub.Name GitHub.Repo -> Config
mkConfig auth' name' repo' =
  Config
  { _cAuth = auth'
  , _cName = name'
  , _cRepo = repo'
  }
