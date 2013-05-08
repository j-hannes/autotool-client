module Model.Types.TaskInstance
    ( TaskInstance (..)
    ) where

import           Model.Indexable

data TaskInstance = TaskInstance {
    -- ^ Identifier
    taskInstanceId        :: Integer

    -- ^ Relations
  , taskInstanceTaskId    :: Integer
  , taskInstanceStudentId :: Integer

    -- ^ Attributes
  , taskInstanceDescription   :: String
  , taskInstanceDocumentation :: String
  , taskInstanceSolution      :: String
  , taskInstanceSignature     :: String
  } deriving (Eq, Read, Show)


instance Indexable TaskInstance where
  iid = taskInstanceId
  setId assn idVal = assn { taskInstanceId = idVal }
