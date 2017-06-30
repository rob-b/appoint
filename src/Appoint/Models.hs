{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}


module Appoint.Models where

import           Appoint.Entities        (runDb)
import qualified Appoint.Entities        as Entities
import           Appoint.IssueStatus     (IssueStatus (..))
import           Appoint.Types.Config
import           Appoint.Types.Issues    hiding (issueLabels)
import           Control.Monad           (forM, forM_)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (MonadReader)
import           Data.Foldable           (toList)
import           Data.Int                (Int64)
import           Data.Monoid             ((<>))
import qualified Data.Vector             as V
import           Database.Esqueleto
import qualified Database.Persist        as P
import           Database.Persist.Sql    (toSqlKey)
import           GitHub.Data.Definitions (IssueLabel)
import           GitHub.Data.Issues      (Issue, issueLabels)


-------------------------------------------------------------------------------
updateTo :: PersistField typ => EntityField v typ -> typ -> P.Update v
updateTo a b = a P.=. b


-------------------------------------------------------------------------------
keyer :: (Integral a, ToBackendKey SqlBackend record) => a -> Key record
keyer i = toSqlKey (fromIntegral i)


-------------------------------------------------------------------------------
labelToLabel :: IssueLabel -> Entities.Label
labelToLabel label =
  Entities.Label (labelName' label) (labelColor' label) (labelUrl' label)


-------------------------------------------------------------------------------
issueToIssue :: Issue -> Entities.Issue
issueToIssue issue =
  Entities.Issue
  { Entities.issueName = issueTitle issue
  , Entities.issueNumber = issueNumber issue
  , Entities.issueUpdatedAt = issueUpdatedAt issue
  , Entities.issueBody = issueBody issue
  , Entities.issueState = issueState issue
  }


-------------------------------------------------------------------------------
persistLabel
  :: (MonadIO m)
  => IssueLabel -> SqlPersistT m (Key Entities.Label)
persistLabel label = do
  let label' = labelToLabel label
  entity <- upsert label' [Entities.LabelName `updateTo` labelName' label]
  return $ entityKey entity


-------------------------------------------------------------------------------
persistIssue
  :: (MonadIO m)
  => Issue -> SqlPersistT m (Key Entities.Issue)
persistIssue issue = do
  let issue' = issueToIssue issue
  entity <- upsert issue' [Entities.IssueUpdatedAt `updateTo` issueUpdatedAt issue]
  return $ entityKey entity


-------------------------------------------------------------------------------
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


-------------------------------------------------------------------------------
saveLabelsFromIssue
  :: MonadIO m
  => Issue
  -> SqlPersistT m (V.Vector (Key Entities.Label))
saveLabelsFromIssue issue = forM (issueLabels issue) persistLabel


-------------------------------------------------------------------------------
-- | Given a collection of issues, save them and any labels they have to the db
saveIssues :: (MonadIO m, Foldable t) => t Issue -> SqlPersistT m ()
saveIssues issues =
  forM_ issues $ \issue -> do
    labelIds <- saveLabelsFromIssue issue
    issueId' <- persistIssue issue
    persistIssueLabel issueId' labelIds


-------------------------------------------------------------------------------
closeIssues
  :: (MonadIO m, Foldable t, Functor t)
  => t Issue -> SqlPersistT m Int64
closeIssues issues =
  let issueIds = toList $ fmap issueNumber issues
  in updateCount $ \issue -> do
       set issue [Entities.IssueState =. val Closed]
       where_ $ issue ^. Entities.IssueState ==. val Open
       where_ $ issue ^. Entities.IssueNumber `notIn` valList issueIds


-------------------------------------------------------------------------------
balanceIssues
  :: (Functor t, Foldable t, MonadReader AppState m, MonadIO m)
  => t Issue -> m ()
balanceIssues issues =
  runDb $ do
    issuesClosed <- closeIssues issues
    liftIO . putStrLn $ "Closed " <> show issuesClosed <> " PR(s)"
    liftIO . putStrLn $ "Found " <> show (length issues) <> " open PR(s)"
    saveIssues issues
