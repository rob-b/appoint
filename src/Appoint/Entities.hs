{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards #-}

module Appoint.Entities where
import Data.Text               (Text)
import Database.Esqueleto
import Database.Persist.TH
import Appoint.IssueStatus


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Label sql=labels
  name Text
  UniqueName name
  color Text
  url Text
  deriving Show

Issue sql=issues
  name Text
  number Int
  UniqueNumber number
  updatedAt Text sql=updated_at
  body Text
  state IssueStatus
  deriving Show

IssueLabel sql=issue_labels
  labelId LabelId sql=label_id
  issueId IssueId sql=issue_id
  IssueLabelIds labelId issueId

Repo sql=repos
  name Text
  owner Text
  externalId Int sql=external_id
  deriving Show
|]
