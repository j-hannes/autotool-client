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
import           Data.IORef            (IORef, newIORef)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Monoid
------------------------------------------------------------------------------
import           Heist
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
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
  , ("/student/select",                           handleStudentSelection)
  , ("/student/:studentId",                       handleStudent)
  , ("/student/:studentId/enroll/:groupId",       handleEnrollment)
  , ("/student/:studentId/enrollments",           showEnrollments)
  , ("/student/:studentId/solve/:taskInstanceId", showSolveTaskForm)
  , ("/course/create",                            handleCourseForm)
  , ("/assign_task",                              handleAssignTask)
  , ("/task/select",                              handleTaskTree)
  , ("/task/configure/:taskname",                 handleTaskConfig)
  , ("/404",                                      render "404")
  , ("",                                          serveDirectory "static")
  ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h  <- nestSnaplet "" heist $ heistInit' "templates" config
    a  <- getNewIORef
    c  <- getNewIORef
    e  <- getNewIORef
    g  <- getNewIORef
    so <- getNewIORef
    st <- getNewIORef
    t  <- getNewIORef
    ti <- getNewIORef
    tu <- getNewIORef
    addRoutes routes
    return $ App h a c e g so st t ti tu
  where
    config = mempty { hcInterpretedSplices = defaultInterpretedSplices }


------------------------------------------------------------------------------
getNewIORef :: Initializer App App (IORef (Map Integer a))
getNewIORef = liftIO . newIORef $ Map.fromList []
