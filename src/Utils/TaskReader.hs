module Utils.TaskReader
    ( getAutotoolTask
    ) where

import           Model.Types.Task (Task (..))


------------------------------------------------------------------------------
-- | 
getAutotoolTask :: String -> IO Task
getAutotoolTask taskname = -- do
    return $ Task taskname
