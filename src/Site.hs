{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString       (ByteString)
import           Data.IORef (IORef, newIORef)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Monoid
import           Control.Concurrent        (withMVar)
------------------------------------------------------------------------------
import           Database.MongoDB
import           Heist
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet.MongoDB
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           Database.Adapter.FileStore
import           Database.Adapter.IORef           (initUsers)
import           Database.Adapter.Sqlite          (createTables)
import           Modules.Student.Controller.Enrollment (handleEnrollment)
import           Modules.Student.Controller.Enrollment (showEnrollments)
import           Modules.Student.Controller.Main       (handleStudent)
import           Modules.Student.Controller.Main       (handleStudentSelection)
import           Modules.Student.Controller.Solution   (showSolveTaskForm)
import           Modules.Tutor.Controller.Assignment   (handleAssignTask)
import           Modules.Tutor.Controller.Course       (handleCourseForm)
import           Modules.Tutor.Controller.Tasks        (showTaskList)
import           Modules.Tutor.Controller.Main         (handleTutor)
import           Modules.Tutor.Controller.TaskConfig   (handleTaskConfig)
import           Modules.Tutor.Controller.TaskTree     (handleTaskTree)


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
    ("/",                                         ifTop $ render "index")
  , ("/tutor",                                    handleTutor)
  , ("/tutor/tasks",                              showTaskList)
  , ("/course/create",                            handleCourseForm)
  , ("/assign_task",                              handleAssignTask)
  , ("/task/select",                              handleTaskTree)
  , ("/task/configure/:taskname",                 handleTaskConfig)
  , ("/student/select",                           handleStudentSelection)
  , ("/student/:studentId",                       handleStudent)
  , ("/student/:studentId/enroll/:groupId",       handleEnrollment)
  , ("/student/:studentId/enrollments",           showEnrollments)
  , ("/student/:studentId/solve/:taskInstanceId", showSolveTaskForm)
  , ("/404",                                      render "404")
  , ("",                                          serveDirectory "static")
  ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit' "templates" config
    
    -- Database.Adapter.IORef
    co <- getNewIORef
    gr <- getNewIORef
    en <- getNewIORef
    ta <- getNewIORef
    as <- getNewIORef
    ti <- getNewIORef
    so <- getNewIORef
    tu <- getNewIORef
    st <- getNewIORef
    liftIO $ initUsers tu st

    -- Database.Adapter.FileStore
    liftIO createFiles  

    -- Database.Adapter.Sqlite
    d <- nestSnaplet "db" db sqliteInit
    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> createTables conn

    -- Database.Adapter.MongoDB
    d2 <- nestSnaplet "database" db2 $ mongoDBInit 10 (host "127.0.0.1")
                     "autotool"
    -- liftIO $ createTables

    addRoutes routes
    return $ App h co gr en ta as ti so tu st d d2
    
  where
    config = mempty { hcInterpretedSplices = defaultInterpretedSplices }


------------------------------------------------------------------------------
-- To enable the Database.Adapter.IORef:
------------------------------------------------------------------------------
getNewIORef :: Initializer App App (IORef (Map String a))
getNewIORef = liftIO . newIORef $ Map.fromList []
