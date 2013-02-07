{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for creating tasks.
module Controller.TaskTree
    ( handleTaskTree
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import           Snap
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application           (AppHandler)
import qualified Autotool.Client       as Autotool
import qualified Autotool.Mock         as AutotoolMock
import qualified View.Task             as View


------------------------------------------------------------------------------
-- | Renders a tree (list) of all available tasks of the autotool.
handleTaskTree :: AppHandler ()
handleTaskTree = do
    request <- getRequest
    let mock = BS.isInfixOf "mock"  $ rqQueryString request
    taskTree <- liftIO $ if mock
                           then AutotoolMock.getTaskTypes
                           else Autotool.getTaskNames
    -- for debugging use
    -- writeText $ T.pack $ init (show taskTrees) ++ " ... ]"
    heistLocal
      (View.bindTaskTreeSplice taskTree)
      (render View.taskTreeTemplate)
