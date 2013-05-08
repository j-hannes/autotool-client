module Model.Adapter.File.Task where

import Control.Applicative ((<$>))
import Data.Time (UTCTime)

import           Autotool.Client.Types.ScoringOrder
import Model.Adapter.File as SimpleStore
import Model.Types.Task

type TutorId = Integer

create :: TutorId -> String -> String -> String -> ScoringOrder -> UTCTime
       -> IO Task
create tid name ttpe sig so time =
    SimpleStore.create "tasks" task
  where
    task = Task 0 tid name ttpe sig so time

-- PRIMARY KEY
getById :: Integer -> IO Task
getById sid = head <$> query taskId sid

getAllByTutorId :: Integer -> IO [Task]
getAllByTutorId = query taskTutorId

query :: (Task -> Integer) -> Integer -> IO [Task]
query foreignId objectId = fmap (filter filterFn) (restoreAll "tasks")
  where
    filterFn = \task -> foreignId task == objectId
