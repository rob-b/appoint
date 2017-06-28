{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}


module Appoint.Models where

import           Appoint.Entities            (runDb)
import qualified Appoint.Entities            as Entities
import           Appoint.Types.Config
import           Appoint.Types.Issues        hiding (issueLabels)
import           Control.Monad               (forM, forM_)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader)
import           Data.Foldable               (toList)
import           Data.Maybe                  (listToMaybe)
import qualified Data.Vector                 as V
import           Database.Esqueleto
import qualified Database.Persist            as P
import           Database.Persist.Sql        (toSqlKey)
import           GitHub.Data.Definitions     (IssueLabel)
import           GitHub.Data.Issues          (Issue, issueLabels)


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


persistLabel
  :: (MonadIO m, MonadReader AppState m)
  => IssueLabel -> m (Key Entities.Label)
persistLabel label = runDb (persistLabel' label)


persistLabel'
  :: (MonadIO m)
  => IssueLabel -> SqlPersistT m (Key Entities.Label)
persistLabel' label = do
  let label' = labelToLabel label
  entity <- upsert label' [Entities.LabelName `updateTo` labelName' label]
  return $ entityKey entity


persistIssue :: (MonadIO m) => Issue -> SqlPersistT m (Key Entities.Issue)
persistIssue issue = do
    let issue' = issueToIssue issue
    entity <- upsert issue' [Entities.IssueUpdatedAt `updateTo` issueUpdatedAt issue]
    return $ entityKey entity


persistIssueLabel
  :: (MonadIO m, Traversable t)
  => Key Entities.Issue
  -> t (Key Entities.Label)
  -> SqlPersistT m (t (Maybe (Key Entities.IssueLabel)))
persistIssueLabel issueId' labelIds =
    forM labelIds $
      \id' -> do
        let issueLabel =
              Entities.IssueLabel
              { Entities.issueLabelIssueId = issueId'
              , Entities.issueLabelLabelId = id'
              }
        insertUnique issueLabel


saveLabelsFromIssue
  :: (MonadIO m, MonadReader AppState m)
  => Issue -> m (V.Vector (Key Entities.Label))
saveLabelsFromIssue issue = forM (issueLabels issue) persistLabel


-------------------------------------------------------------------------------
-- | Given a collection of issues, save them and any labels they have to the db
saveIssues
  :: (Functor t, Foldable t, MonadReader AppState m, MonadIO m)
  => t Issue -> m ()
saveIssues issues = do
  existing <- countExisting issues
  liftIO $ print existing
  forM_ issues $ \issue -> do
    labelIds <- saveLabelsFromIssue issue
    issueId' <- runDb (persistIssue issue)
    runDb (persistIssueLabel issueId' labelIds)


countExisting
  :: (MonadReader AppState m, MonadIO m, Foldable t, Functor t)
  => t Issue -> m Int
countExisting issues = do
  existing <- listToMaybe <$> runDb (countExisting' issues)
  pure $ maybe 0 unValue existing


countExisting'
  :: (MonadIO m, Foldable t, Num a, Functor t, PersistField a)
  => t Issue -> SqlPersistT m [Value a]
countExisting' issues =
  let issueIds = toList $ fmap issueNumber issues
  in select . from $ \issue -> do
       where_ $ issue ^. Entities.IssueNumber `notIn` valList issueIds
       return $ count (issue ^. Entities.IssueId)
