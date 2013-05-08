module Model.Types.Enrollment where

import Data.Time (UTCTime)
import Model.Types.Reference

data Enrollment = Enrollment
  { enrollmentGroup     :: Reference
  , enrollemntStudent   :: Reference
  , enrollemntTimestamp :: UTCTime
  } deriving (Eq, Read, Show)
