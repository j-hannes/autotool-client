module Model.Types.Enrollment
    ( Enrollment (..)
    ) where

import           Data.Time (UTCTime)
import           Model.Indexable

data Enrollment = Enrollment {
    -- ^ Identifier
    enrollmentId        :: Integer

    -- ^ Relations
  , enrollmentGroupId   :: Integer
  , enrollmentStudentId :: Integer

    -- ^ Attributes
  , enrollmentTime      :: UTCTime
  } deriving (Eq, Read, Show)

instance Indexable Enrollment where
  iid = enrollmentId
  setId assn idVal = assn { enrollmentId = idVal }
