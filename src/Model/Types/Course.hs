module Model.Types.Course where

import           Data.Time
import           Model.Indexable

data Course = Course {
    -- ^ Identifier
    courseId             :: Integer

    -- ^ Relations
  , courseTutorId        :: Integer

    -- ^ Attributes
  , courseName           :: String
  , courseSemester       :: String
  , courseEnrollmentFrom :: Maybe UTCTime
  , courseEnrollmentTo   :: Maybe UTCTime
  , coursePassCriteria   :: Double
  } deriving (Eq, Ord, Read, Show)


instance Indexable Course where
  iid = courseId
  setId course idVal = course { courseId = idVal }
