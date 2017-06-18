{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}


module Appoint.Models where

import           Appoint.Entities        (migrateAll, runDb)
import qualified Appoint.Entities        as Entities
import           Appoint.Types.Issues    hiding (issueLabels)
import           Control.Monad           (forM, forM_)
import qualified Data.Vector             as V
import           Database.Esqueleto
import qualified Database.Persist        as P
import           Database.Persist.Sql    (toSqlKey)
import           Database.Persist.Sqlite (runSqlite, createSqlitePool)
import           GitHub.Data.Definitions (IssueLabel)
import           GitHub.Data.Issues      (Issue, issueLabels)
import Data.Foldable (toList)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)


updateTo :: PersistField typ => EntityField v typ -> typ -> P.Update v
updateTo a b = a P.=. b


keyer :: (Integral a, ToBackendKey SqlBackend record) => a -> Key record
keyer i = toSqlKey (fromIntegral i)


labelToLabel :: IssueLabel -> Entities.Label
labelToLabel label =
  Entities.Label (labelName' label) (labelColor' label) (labelUrl' label)


issueToIssue :: Issue -> Entities.Issue
issueToIssue issue =
  Entities.Issue
  { Entities.issueName = issueTitle issue
  , Entities.issueNumber = issueNumber issue
  , Entities.issueUpdatedAt = issueUpdatedAt issue
  , Entities.issueBody = issueBody issue
  , Entities.issueState = issueState issue
  }


persistLabel :: IssueLabel -> IO (Key Entities.Label)
persistLabel label =
  runSqlite "pr.sqlite" $
  do runMigration migrateAll
     let label' = labelToLabel label
     entity <- upsert label' [Entities.LabelName `updateTo` labelName' label]
     return $ entityKey entity


persistIssue :: Issue -> IO (Key Entities.Issue)
persistIssue issue =
  runSqlite "pr.sqlite" $
  do runMigration migrateAll
     let issue' = issueToIssue issue
     entity <- upsert issue' [Entities.IssueUpdatedAt `updateTo` issueUpdatedAt issue]
     return $ entityKey entity


persistIssueLabel
  :: (Traversable t)
  => Key Entities.Issue
  -> t (Key Entities.Label)
  -> IO (t (Maybe (Key Entities.IssueLabel)))
persistIssueLabel issueId' labelIds =
  runSqlite "pr.sqlite" $
  do runMigration migrateAll
     forM labelIds $
       \id' -> do
         let issueLabel =
               Entities.IssueLabel
               { Entities.issueLabelIssueId = issueId'
               , Entities.issueLabelLabelId = id'
               }
         insertUnique issueLabel


saveLabelsFromIssue :: Issue -> IO (V.Vector (Key Entities.Label))
saveLabelsFromIssue issue = forM (issueLabels issue) persistLabel


-------------------------------------------------------------------------------
-- | Given a collection of issues, save them and any labels they have to the db
saveIssues
  :: (Functor t, Foldable t, MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => t Issue -> m ()
saveIssues issues = do
  existing <- countExisting issues
  liftIO $ print existing
  forM_ issues $ \issue -> do
    labelIds <- liftIO $ saveLabelsFromIssue issue
    issueId' <- liftIO $ persistIssue issue
    liftIO $ persistIssueLabel issueId' labelIds


countExisting
  :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, Foldable t, Functor t)
  => t Issue -> m Int
countExisting issues = do
  pool <- createSqlitePool "pr.sqlite" 1
  [existing] <- runDb pool (countExisting' issues)
  pure $ unValue existing


countExisting'
  :: (MonadIO m, Foldable t, Num a, Functor t, PersistField a)
  => t Issue -> SqlPersistT m [Value a]
countExisting' issues =
  let issueIds = toList $ fmap issueNumber issues
  in select . from $ \issue -> do
       where_ $ issue ^. Entities.IssueNumber `notIn` valList issueIds
       return $ count (issue ^. Entities.IssueId)
