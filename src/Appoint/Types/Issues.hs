{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Appoint.Types.Issues where

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GitHub.Data.Issues as GitHub
import qualified GitHub.Data.PullRequests as GitHub
import qualified GitHub.Data.Milestone as GitHub
import qualified GitHub.Data.Definitions as GitHub
import qualified GitHub.Data.URL as GitHub
import qualified GitHub.Data.Name as GitHub
import qualified GitHub.Data.Id as GitHub
import GitHub.Data.Options (IssueState(..))
import Data.Vector (Vector)
import Appoint.Import (IssueStatus(Open), IssueStatus(Closed))


instance ToJSON GitHub.Issue where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance ToJSON GitHub.IssueLabel where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance ToJSON GitHub.PullRequestReference where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance ToJSON GitHub.Milestone where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance ToJSON GitHub.SimpleUser where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }


labelColor' :: GitHub.IssueLabel -> Text
labelColor' = GitHub.labelColor


labelUrl' :: GitHub.IssueLabel -> Text
labelUrl' label = GitHub.getUrl $ GitHub.labelUrl label


labelName' :: GitHub.IssueLabel -> Text
labelName' label = GitHub.untagName $ GitHub.labelName label


issueUpdatedAt :: GitHub.Issue -> Text
issueUpdatedAt issue = T.pack . show $ GitHub.issueUpdatedAt issue


issueLabels :: GitHub.Issue -> Vector GitHub.IssueLabel
issueLabels = GitHub.issueLabels


issueNumber :: GitHub.Issue -> Int
issueNumber = GitHub.issueNumber


issueTitle :: GitHub.Issue -> Text
issueTitle = GitHub.issueTitle


issueBody :: GitHub.Issue -> Text
issueBody issue = fromMaybe "" (GitHub.issueBody issue)


issueId :: GitHub.Issue -> Int
issueId issue = GitHub.untagId $ GitHub.issueId issue


issueState :: GitHub.Issue -> IssueStatus
issueState issue =
  case GitHub.issueState issue of
    StateOpen -> Open
    StateClosed -> Closed
