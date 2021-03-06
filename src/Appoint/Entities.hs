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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks, MonadReader)
import Appoint.Types.Config (AppState, appPool)


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
  repoId RepoId sql=repo_id
  deriving Show

IssueLabel sql=issue_labels
  labelId LabelId sql=label_id
  issueId IssueId sql=issue_id
  IssueLabelIds labelId issueId

Repo sql=repos
  name Text
  owner Text
  externalId Int sql=external_id
  OwnerName owner name
  deriving Show
|]


doMigrations :: (MonadIO m) => SqlPersistT m ()
doMigrations = runMigration migrateAll


runDb
  :: (MonadIO m, MonadReader AppState m)
  => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks appPool
  liftIO $ runSqlPool query pool
