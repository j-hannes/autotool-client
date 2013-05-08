module Model.Types.Timespan where

import Data.Time (UTCTime)

data Timespan = Timespan
  { timespanStart :: UTCTime
  , timespanEnd   :: UTCTime
  } deriving (Eq, Read, Show)
