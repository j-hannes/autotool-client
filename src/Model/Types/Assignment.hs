module Model.Types.Assignment
    ( Assignment (..)
    , Status (..)
    ) where

import           Data.Time (UTCTime)
import           Model.Indexable

data Assignment = Assignment
  { aid      :: Maybe Int
  , courseId :: Int
  , taskId   :: Int
  , status   :: Status
  , start    :: UTCTime
  , end      :: UTCTime
  } deriving (Read, Show)

data Status = Mandatory
            | Optional
            deriving (Read, Show, Eq)


instance Indexable Assignment where
  iid = aid
  setId assn idVal = assn { aid = Just idVal }
