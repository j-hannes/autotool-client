------------------------------------------------------------------------------
-- | Data type that represents a successfully configured task.
--
-- The name and signature of it can be sent to the autotool server to receive
-- a task instance which contains task description, documentation, example
-- solution etc.
--
module Model.Types.Task where

import           Data.Time

import           Autotool.Client.Types.ScoringOrder
import           Model.Indexable


data Task = Task {
    -- ^ Identifier
      taskId           :: Integer

    -- ^ Relations
    , taskTutorId      :: Integer

    -- ^ Attributes
    , taskName         :: String
    , taskType         :: String
    , taskSignature    :: String
    , taskScoringOrder :: ScoringOrder
    , taskCreated      :: UTCTime
    } deriving (Read, Show)


instance Indexable Task where
  iid = taskId
  setId taskconfig idVal = taskconfig { taskId = idVal }
