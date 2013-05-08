module Model.Types.Solution where

import Data.Time (UTCTime)
import Model.Types.Reference

data Solution = Solution
  { solutionTaskInstance :: Reference
  , solutionContent      :: String
  , solutionScore        :: Int
  , solutionSize         :: Int
  , solutionTimestamp    :: UTCTime
  } deriving (Eq, Read, Show)
