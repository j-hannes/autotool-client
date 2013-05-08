------------------------------------------------------------------------------
-- | Data type that represents a successfully configured solution.
--
-- The name and signature of it can be sent to the autotool server to receive
-- a solution instance which contains solution description, documentation,
-- example solution etc.
--
module Model.Types.Solution where

import           Data.Time

import           Model.Indexable


data Solution = Solution {
    -- ^ Identifier
      solutionId             :: Integer

    -- ^ Relations
    , solutionTaskInstanceId :: Integer

    -- ^ Attributes
    , solutionContent        :: String
    , solutionEvaluation     :: String
    , solutionResult         :: Maybe Result
    , solutionSubmission     :: UTCTime
    } deriving (Read, Show)

data Result = Result {
      score :: Int
    , size  :: Int
    } deriving (Eq, Read, Show)

instance Indexable Solution where
  iid = solutionId
  setId solutionconfig idVal = solutionconfig { solutionId = idVal }
