{-# LANGUAGE TemplateHaskell #-}
module Appoint.IssueStatus where

import Database.Persist.TH
data IssueStatus = Open | Closed
    deriving (Show, Read, Eq)

derivePersistField "IssueStatus"

