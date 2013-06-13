{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString           (ByteString)
import           Data.IORef                (IORef, newIORef)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid
import           Control.Concurrent        (withMVar)
------------------------------------------------------------------------------
import           Heist
import           Snap
import           Snap.Snaplet.AcidState    (acidInit)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           Model.DbAdapter.FileStore
import           Model.DbAdapter.IORef                 (initUsers)
import           Model.DbAdapter.Sqlite                (createTables)
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
    h <- nestSnaplet "" heist $ heistInit' "templates" config
    
    -- Model.DbAdapter.IORef
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

    -- Model.DbAdapter.FileStore
    liftIO createFiles  

    -- Model.DbAdapter.Sqlite
    d <- nestSnaplet "db" db sqliteInit
    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> createTables conn

    au <- nestSnaplet "au" acid_tutors        $ initTutorStore
    an <- nestSnaplet "as" acid_students      $ initStudentStore

    ac <- nestSnaplet "ac" acid_courses       $ initCourseStore
    ag <- nestSnaplet "ag" acid_groups        $ initGroupStore
    ae <- nestSnaplet "ae" acid_enrollments   $ initEnrollmentStore

    at <- nestSnaplet "at" acid_tasks         $ initTaskStore
    aa <- nestSnaplet "aa" acid_assignments   $ initAssignmentStore
    ai <- nestSnaplet "ai" acid_taskInstances $ initTaskInstanceStore
    ao <- nestSnaplet "ao" acid_solutions     $ initSolutionStore

    addRoutes routes
    return $ App h co gr en ta as ti so tu st d au an ac ag ae at aa ai ao
    
  where
    config = mempty { hcInterpretedSplices = defaultInterpretedSplices }
    initAssignmentStore   = acidInit (AssignmentStore   $ Map.fromList [])
    initCourseStore       = acidInit (CourseStore       $ Map.fromList [])
    initEnrollmentStore   = acidInit (EnrollmentStore   $ Map.fromList [])
    initGroupStore        = acidInit (GroupStore        $ Map.fromList [])
    initSolutionStore     = acidInit (SolutionStore     $ Map.fromList [])
    initTaskStore         = acidInit (TaskStore         $ Map.fromList [])
    initTaskInstanceStore = acidInit (TaskInstanceStore $ Map.fromList [])
    initTutorStore        = acidInit (TutorStore        $ Map.fromList [])
    initStudentStore      = acidInit (StudentStore      $ Map.fromList [])


------------------------------------------------------------------------------
-- To enable the Model.DbAdapter.IORef:
------------------------------------------------------------------------------
getNewIORef :: Initializer App App (IORef (Map Integer a))
getNewIORef = liftIO . newIORef $ Map.fromList []
