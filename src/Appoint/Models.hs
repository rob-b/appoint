{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}


module Appoint.Models where
import           Database.Esqueleto
-- import           Database.Persist hiding ((=.))
import qualified Database.Persist as P
import           Database.Persist.Sql    (toSqlKey)
import           Database.Persist.Sqlite (runSqlite)
import           Appoint.Entities             (migrateAll)
import qualified Appoint.Entities             as Entities
import Appoint.Types.Issues
import GitHub.Data.Definitions (IssueLabel)
import GitHub.Data.Issues (Issue)
import Control.Monad (forM)


updateTo :: PersistField typ => EntityField v typ -> typ -> P.Update v
updateTo a b = a P.=. b


keyer :: (Integral a, ToBackendKey SqlBackend record) => a -> Key record
keyer i = toSqlKey (fromIntegral i)


labelToLabel :: IssueLabel -> Entities.Label
labelToLabel label =
  Entities.Label (labelName' label) (labelColor' label) (labelUrl' label)


issueToIssue issue =
  Entities.Issue
  { Entities.issueName = issueTitle issue
  , Entities.issueNumber = issueNumber issue
  , Entities.issueUpdatedAt = issueUpdatedAt issue
  , Entities.issueBody = issueBody issue
  }


persistLabel :: IssueLabel -> IO (Key Entities.Label)
persistLabel label =
  runSqlite "pr.sqlite" $
  do runMigration migrateAll
     insert (labelToLabel label)


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
  -> IO (t (Key Entities.IssueLabel))
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
         insert issueLabel
