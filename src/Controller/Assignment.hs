{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This controller enables the assignments between courses and tasks.
module Controller.Assignment
    ( handleAssignTask
    ) where

------------------------------------------------------------------------------
import           Data.Maybe                    (fromJust)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Snap                          (liftIO, (<$>), (<*>), redirect)
import           Text.Digestive.Form           (Form, choice, text, (.:),
                                                check)
import           Text.Digestive.Snap           (runForm)
------------------------------------------------------------------------------
import           Application
import qualified Model.Adapter.File.Assignment as Assignment
import qualified Model.Adapter.File.Course     as Course
import qualified Model.Adapter.File.Task       as Task
import           Model.Types.Assignment
import           Model.Types.Course
import           Model.Types.Task
import           Utils.Form                    (renderForm, convertDate)


------------------------------------------------------------------------------
-- | Handler to assign a task to a course.
handleAssignTask :: AppHandler ()
handleAssignTask = do
  courses <- liftIO $ Course.getAllByTutorId 1
  tasks   <- liftIO $ Task.getAllByTutorId 1
  let courseIds = map (tupleIdName courseId courseName) courses
      taskIds   = map (tupleIdName taskId taskName) tasks
  (view, assignData) <- runForm "form" (assignForm courseIds taskIds)
  maybe (renderForm "tutor/forms/assignment" view) createAssignment assignData


------------------------------------------------------------------------------
-- | Converts an object that has id and name attributes into a tuple of those.
tupleIdName :: (a -> Integer) -> (a -> String) -> a -> (Integer, Text)
tupleIdName idFn nameFn obj = (idFn obj, T.pack $ nameFn obj)


------------------------------------------------------------------------------
-- | Data type for assignment form.
data AssignmentData = AssignmentData
  { formCourseId :: Integer
  , formTaskId   :: Integer
  , formStatus   :: Status
  , formStart    :: Text
  , formEnd      :: Text
  } deriving (Show)


------------------------------------------------------------------------------
-- | The form to create an assignment, filles with courses and tasks to
-- select.
assignForm :: [(Integer, Text)] -> [(Integer, Text)]
           -> Form Text AppHandler AssignmentData
assignForm courses tasks = AssignmentData
    <$> "course" .: choice courses Nothing
    <*> "task"   .: choice tasks Nothing
    <*> "status" .: choice status' Nothing
    <*> "start"  .: check missingStartDateMsg notEmpty (text Nothing)
    <*> "end"  .: check missingEndDateMsg notEmpty (text Nothing)
  where
    status' = [(Mandatory, "Pflicht"), (Optional, "Zusatz")]

missingStartDateMsg :: Text
missingStartDateMsg = "Bitte ein Startzeitpunkt angeben."

missingEndDateMsg :: Text
missingEndDateMsg = "Bitte einen Endzeitpunkt angeben."

notEmpty :: Text -> Bool
notEmpty t = t /= ""

------------------------------------------------------------------------------
-- | Create an assignment if all teh data is collected.
createAssignment :: AssignmentData -> AppHandler()
createAssignment ad =
    liftIO (Assignment.create cid tid sts start end) >> redirect "/tutor"
  where
    cid   =                          formCourseId ad
    tid   =                          formTaskId   ad
    sts   =                          formStatus   ad
    start = fromJust . convertDate $ formStart    ad
    end   = fromJust . convertDate $ formEnd      ad
