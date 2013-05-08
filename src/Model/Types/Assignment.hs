module Model.Types.Assignment
    ( Assignment (..)
    , Status (..)
    ) where

import           Data.Time (UTCTime)
import           Model.Indexable

data Assignment = Assignment {
    -- ^ Identifier
    assignmentId       :: Integer

    -- ^ Relations
  , assignmentCourseId :: Integer
  , assignmentTaskId   :: Integer

    -- ^ Attributes
  , assignmentStatus   :: Status
  , assignmentStart    :: UTCTime
  , assignmentEnd      :: UTCTime
  } deriving (Eq, Read, Show)

data Status = Mandatory
            | Optional
            deriving (Read, Show, Eq)


instance Indexable Assignment where
  iid = assignmentId
  setId assn idVal = assn { assignmentId = idVal }
