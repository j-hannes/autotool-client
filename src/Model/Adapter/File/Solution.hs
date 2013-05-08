module Model.Adapter.File.Solution where

import Data.Time (UTCTime)

import Model.Adapter.File as SimpleStore
import Model.Types.Solution

type TaskInstanceId = Integer

create :: TaskInstanceId -> String -> String -> Maybe Result -> UTCTime
       -> IO Solution
create tid cont eval res time =
    SimpleStore.create "solutions" solution
  where
    solution = Solution 0 tid cont eval res time

-- PRIMARY KEY
getAllByTaskInstanceId :: Integer -> IO [Solution]
getAllByTaskInstanceId = query solutionTaskInstanceId

query :: (Solution -> Integer) -> Integer -> IO [Solution]
query foreignId objectId = fmap (filter filterFn) (restoreAll "solutions")
  where
    filterFn = \solution -> foreignId solution == objectId
