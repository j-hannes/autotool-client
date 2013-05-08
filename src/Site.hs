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
import           Data.Monoid
import           Heist
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           Controller.Assignment (handleAssignTask)
import           Controller.Course     (handleCourseForm)
import           Controller.Enrollment (handleEnrollment, showEnrollments)
import           Controller.Tasks      (showTaskList)
import           Controller.Tutor      (handleTutor)
import           Controller.Student    (handleStudent)
import           Controller.Student    (handleStudentSelection)
import           Controller.Solution   (showSolveTaskForm)
import           Controller.TaskConfig (handleTaskConfig)
import           Controller.TaskTree   (handleTaskTree)


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
  , ("",                                          serveDirectory "static")
  ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit' "templates" config
    addRoutes routes
    return $ App h
  where
    config = mempty { hcInterpretedSplices = defaultInterpretedSplices }
