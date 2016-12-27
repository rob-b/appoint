{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Appoint.Types.Users where
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.Text as T

data User = User
  { profileURL :: T.Text
  , userName :: T.Text
  } deriving (Show, Generic)

data Collaborators = Collaborators
  { _path :: T.Text
  , _users :: [User]
  } deriving (Show,Generic)

makeLenses ''Collaborators

instance ToJSON User where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON User where
  parseJSON (Object u) = User <$> u .: "profile_url" <*> u .: "user_name"
  parseJSON invalid = typeMismatch "User" invalid

instance ToJSON Collaborators where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }
instance FromJSON Collaborators where
  parseJSON (Object c) = Collaborators <$> c .: "path" <*> c .: "users"
  parseJSON invalid = typeMismatch "Collaborators" invalid
