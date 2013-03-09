module Model.Types.Course where

import           Data.Time
import           Model.Indexable

data Course = Course {
    cid             :: Maybe Int
  , courseName      :: String
  , semester        :: String
  , enrollmentBegin :: Maybe UTCTime
  , enrollmentEnd   :: Maybe UTCTime
  , passCriteria    :: Double
  } deriving (Read, Show)


instance Indexable Course where
  iid = cid
  setId course idVal = course { cid = Just idVal }
