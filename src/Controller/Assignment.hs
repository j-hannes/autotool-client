{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This controller enables the assignments between courses and tasks.
module Controller.Assignment
    ( handleAssignTask
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8         as BS
import           Data.Maybe                    (fromJust)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (UTCTime)
import           Snap                          (liftIO, (<$>), (<*>), writeBS,
                                                redirect)
import           Text.Digestive.Form           (Form, choice, text, (.:),
                                                check)
import           Text.Digestive.Snap           (runForm)
------------------------------------------------------------------------------
import           Application
import           Model.Adapter.File
import           Model.Types.Assignment        (Assignment(Assignment),
                                                Status(..))
import qualified Model.Types.Assignment        as Assignment
import           Model.Types.Course            (Course)
import qualified Model.Types.Course            as Course
import           Model.Types.TaskConfig        (TaskConfig)
import qualified Model.Types.TaskConfig        as Task
import           Utils.Form                    (renderForm, convertDate)


------------------------------------------------------------------------------
-- | Handler to assign a task to a course.
handleAssignTask :: AppHandler ()
handleAssignTask = do
  courses <- liftIO $ restoreAll "course"
  tasks   <- liftIO $ restoreAll "taskconfig"
  let courseIds = map (tupleIdName Course.cid Course.courseName) courses
      taskIds   = map (tupleIdName Task.tcid Task.title) tasks
  (view, assignData) <- runForm "form" (assignForm courseIds taskIds)
  maybe (renderForm "tutor/forms/assignment" view) createAssignment assignData


------------------------------------------------------------------------------
-- | Converts an object that has id and name attributes into a tuple of those.
tupleIdName :: (a -> Maybe Int) -> (a -> String) -> a -> (Int, Text)
tupleIdName idFn nameFn obj = (fromJust $ idFn obj, T.pack $ nameFn obj)


------------------------------------------------------------------------------
-- | Data type for assignment form.
data AssignmentData = AssignmentData
  { courseId :: Int
  , taskId :: Int
  , status :: Status
  , start :: Text
  , end :: Text
  } deriving (Show)


------------------------------------------------------------------------------
-- | The form to create an assignment, filles with courses and tasks to
-- select.
assignForm :: [(Int, Text)] -> [(Int, Text)]
           -> Form Text AppHandler AssignmentData
assignForm courses tasks = AssignmentData
    <$> "course" .: choice courses Nothing
    <*> "task"   .: choice tasks Nothing
    <*> "status" .: choice status' Nothing
    <*> "start"  .: check missingStartDateMsg notEmpty (text Nothing)
    <*> "end"  .: check missingEndDateMsg notEmpty (text Nothing)
  where
    status' = [(Mandatory, "Pflicht"), (Optional, "Zusatz")]

missingStartDateMsg = "Bitte ein Startzeitpunkt angeben."
missingEndDateMsg = "Bitte einen Endzeitpunkt angeben."

notEmpty :: Text -> Bool
notEmpty t = t /= ""

------------------------------------------------------------------------------
-- | Create an assignment if all teh data is collected.
createAssignment :: AssignmentData -> AppHandler()
createAssignment ad = do

    let assignment = Assignment {
            Assignment.aid = Nothing
          , Assignment.courseId = courseId ad
          , Assignment.taskId = taskId ad
          , Assignment.status = status ad
          , Assignment.start = fromJust . convertDate $ start ad
          , Assignment.end = fromJust . convertDate $ end ad
          }
    
    _ <- liftIO $ create "assignment" assignment
  
    redirect "/tutor"
