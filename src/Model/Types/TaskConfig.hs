------------------------------------------------------------------------------
-- | Data type that represents a successfully configured task.
--
-- The name and signature of it can be sent to the autotool server to receive
-- a task instance which contains task description, documentation, example
-- solution etc.
--
module Model.Types.TaskConfig
    ( TaskConfig (..)
    ) where

import           Data.Time

import           Autotool.Client.Types.ScoringOrder
import           Model.Indexable
import           Model.Types.Assignment


data TaskConfig = TaskConfig
    { tcid          :: Maybe Int
    , title         :: String
    , name          :: String
    , signature     :: String
    , scoring       :: ScoringOrder
    , created       :: UTCTime
    } deriving (Read, Show)


instance Indexable TaskConfig where
  iid = tcid
  setId taskconfig idVal = taskconfig { tcid = Just idVal }
