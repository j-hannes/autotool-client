module Model.Adapter.Json.TaskConfig
  ( createTask
  ) where

-- import qualified Data.Aeson as JSON
import           Data.Maybe
import           System.IO

import           Model.Types.TaskConfig (TaskConfig)
import qualified Model.Types.TaskConfig as TaskConfig

datafile :: String
datafile = "data/taskconfigs.json"

------------------------------------------------------------------------------
-- | 
createTask :: TaskConfig -> IO ()
createTask task = do
    handle <- openFile datafile ReadMode
    contents <- hGetContents handle
    let tasks = map read (lines contents) :: [TaskConfig]
        nextId = getNextId 1 tasks
        task' = task {TaskConfig.id = Just nextId}

    hClose handle

    handle' <- openFile datafile AppendMode
    hPutStr handle' ("\n" ++ show task')
    hClose handle'


------------------------------------------------------------------------------
getNextId :: Int -> [TaskConfig] -> Int
getNextId n [] = n
getNextId n (x:xs) | isJust i && fromJust i > n = getNextId (fromJust i) xs
                   | otherwise                  = getNextId n xs        
                   where
                     i = TaskConfig.id x
