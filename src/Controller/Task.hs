{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for creating tasks.
module Controller.Task
    ( handleTaskConfig
    , handleTaskTree
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import           Snap
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application
import           Autotool.Client.Interface.Direct
import           Autotool.Client.Types.TaskTree
import qualified View.Task as View


------------------------------------------------------------------------------
-- | Renders a tree (list) of all available tasks of the autotool.
handleTaskTree :: AppHandler ()
handleTaskTree = do
    taskTree <- liftIO $ getTaskTree True
    -- for debugging use
    -- writeText $ T.pack $ init (show taskTrees) ++ " ... ]"
    heistLocal
      (View.bindTaskTreeSplice taskTree)
      (render View.taskTreeTemplate)


------------------------------------------------------------------------------
-- | 
handleTaskConfig :: AppHandler ()
handleTaskConfig = do
    nme  <- fmap BS.unpack $ fromMaybe "error taskname" <$> getParam "taskname"
    heistLocal (I.bindString "taskName" $ T.pack nme) $ render "forms/taskConfig"


------------------------------------------------------------------------------
-- | Switch for local accessibility of an example data structure.
getTaskTree :: Bool -> IO [TaskTree]
getTaskTree fromRemote
    | fromRemote = getTaskTypes
    | otherwise  = do 
        t <- readFile "taskTypes.mock"
        let tree = read (init t) :: [TaskTree]
        return {-take 2 $-} tree
