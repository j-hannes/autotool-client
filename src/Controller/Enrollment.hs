{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This controller enables the enrollments between courses and tasks.
module Controller.Enrollment
    ( handleEnrollment
    , showEnrollments
    ) where

------------------------------------------------------------------------------
import           Control.Monad
import qualified Data.ByteString.Char8         as BS
import           Data.List                     (nub)
import           Data.Maybe                    (fromMaybe)
import qualified Data.Text                     as T
import           Data.Time                     (getCurrentTime)
import           Heist.Interpreted             (Splice)
import qualified Heist.Interpreted             as I
import           Snap                          (liftIO, (<$>), redirect)
import           Snap                          (getParam)
import           Snap.Snaplet.Heist            (heistLocal, render)
------------------------------------------------------------------------------
import           Application
import           Model.Adapter.File.Course     as Course
import           Model.Adapter.File.Enrollment as Enrollment
import           Model.Adapter.File.Group      as Group
import           Model.Types.Course
import           Model.Types.Group
-- import           Model.Types.Task
-- import           Utils.Form                    (renderForm, convertDate)
import           Utils.Auth                    (getStudentId)


------------------------------------------------------------------------------
-- | Handler to enroll a task to a course.
showEnrollments :: AppHandler ()
showEnrollments = do
  -- todo: get all groups, but actually sorted nicely by courses to show them
  -- below each other. allow to enroll only in one of the groups for each
  -- courses. show only courses where the student is not yet enrolled, hehe.
  studentId <- getStudentId
  groups  <- liftIO $ Group.getEnrollableGroups studentId
  courses <- liftIO $ forM (nub $ map groupCourseId groups) Course.getById
  let courseGroups = map (\c -> (c, filter (\g -> groupCourseId g == courseId c) groups)) courses
  let splices = [
          ("courseGroups", I.mapSplices renderCourseGroup courseGroups)
        ]  
  heistLocal (I.bindSplices splices) $ render "student/pages/enrollment"

renderCourseGroup :: (Course, [Group]) -> Splice AppHandler
renderCourseGroup (course, groups) =
    I.runChildrenWith splices
  where
    splices = [
        ("courseName", I.textSplice . T.pack $ courseName course)
      , ("groups", I.mapSplices renderGroup groups)
      ]

renderGroup :: Group -> Splice AppHandler
renderGroup group =
    I.runChildrenWith splices
  where
    splices = [
        ("groupId",          I.textSplice . T.pack . show $ groupId   group)
      , ("groupDescription", I.textSplice . T.pack $ groupDescription group)
      ]

handleEnrollment :: AppHandler ()
handleEnrollment =
    getStudentId                                       >>= \studentId ->
    BS.unpack <$> fromMaybe "0" <$> getParam "groupId" >>= \gid   ->
    liftIO getCurrentTime                              >>= \ time ->
    liftIO (Enrollment.create (read gid) studentId time)  >>
    redirect "/student/enrollments"
  where
