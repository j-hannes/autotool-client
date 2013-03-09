module Model.Adapter.Json.TaskConfig
  ( create
  , restoreAll
  ) where

-- import qualified Data.Aeson as JSON
import           Data.Maybe
import           System.IO hiding (hGetContents)
import           System.IO.Strict (hGetContents)

import           Model.Types.TaskConfig (TaskConfig)
import qualified Model.Types.TaskConfig as TaskConfig

datafile :: String
datafile = "data/taskconfigs.json"

------------------------------------------------------------------------------
-- | 
create :: TaskConfig -> IO TaskConfig
create task = do
    tasks <- restoreAll
    let nextId = getNextId 0 tasks
        task' = task {TaskConfig.id = Just nextId}
        new   = task' : tasks
    handle' <- openFile datafile WriteMode
    hPutStr handle' . unlines $ map show new
    hClose handle'
    return task'

-- restore :: Int -> IO TaskConfig
-- restore _ = undefined

restoreAll :: IO [TaskConfig]
restoreAll = do
    handle <- openFile datafile ReadMode
    contents <- hGetContents handle
    hClose handle
    return . map read $ lines contents


------------------------------------------------------------------------------
getNextId :: Int -> [TaskConfig] -> Int
getNextId n [] = n + 1
getNextId n (x:xs) | isJust i && fromJust i > n = getNextId (fromJust i) xs
                   | otherwise                  = getNextId n xs        
                   where
                     i = TaskConfig.id x
