module Model.Types.Assignment
    ( Assignment (..)
    ) where

import           Data.Time (UTCTime)

data Assignment = Assignment
  { status :: Status
  , start  :: UTCTime
  , end    :: UTCTime
  } deriving (Read, Show)

data Status = Mandatory
            | Optional
            deriving (Read, Show)
