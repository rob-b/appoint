{-# LANGUAGE OverloadedStrings #-}
module Appoint.Users where
import Control.Lens
import Control.Monad.Reader
import Data.Monoid ((<>))
import Appoint.Types.Config
import Appoint.Types.Users (User(..), Collaborators(..))
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified GitHub.Data as GitHub
import qualified GitHub.Endpoints.Repos.Collaborators as GitHub

collaboratorsOn :: ReaderT Config IO (Either GitHub.Error (V.Vector GitHub.SimpleUser))
collaboratorsOn = do
  config <- ask
  liftIO $
    GitHub.collaboratorsOn'
      (config ^. cAuth)
      (config ^. cOwner)
      (config ^. cRepo)

mkUser :: GitHub.SimpleUser -> User
mkUser u =
  User
  { userName = gitHubUserName u
  , profileURL = GitHub.getUrl (GitHub.simpleUserUrl u)
  }

gitHubUserName :: GitHub.SimpleUser -> T.Text
gitHubUserName = GitHub.untagName . GitHub.simpleUserLogin

mkCollaborators
  :: GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> [GitHub.SimpleUser]
  -> [Collaborators]
mkCollaborators name' repo' users' =
  [ Collaborators
    { _path = GitHub.untagName name' <> "/" <> GitHub.untagName repo'
    , _users = map mkUser users'
    }]
