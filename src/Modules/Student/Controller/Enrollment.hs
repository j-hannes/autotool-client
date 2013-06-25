{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This controller enables the enrollments between courses and tasks.
module Modules.Student.Controller.Enrollment
    ( handleEnrollment
    , showEnrollments
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import           Data.Time             (getCurrentTime)
import           Heist.Interpreted     (Splice)
import qualified Heist.Interpreted     as I
import           Snap                  (liftIO, lift, (<$>), redirect)
import           Snap                  (getParam)
import           Snap.Snaplet.Heist    (heistLocal, render)
------------------------------------------------------------------------------
import           Application           (AppHandler)
import qualified Database.Switch       as Database
import           Model.Datatypes
import           Utils.Auth            (getStudentId)
import           Utils.Render          ((|-))


------------------------------------------------------------------------------
-- | Handler to enroll a task to a course.
showEnrollments :: AppHandler ()
showEnrollments = do
  sid      <- getStudentId
  mStudent <- Database.getStudent sid
  case mStudent of
    Nothing      -> redirect "/404"
    Just student -> do
      courses <- Database.getEnrollableCourses student
      let splices = [
              ("studentId", I.textSplice . T.pack $ sid)
            , ("courses",   I.mapSplices renderCourse courses)
            ]  
      heistLocal (I.bindSplices splices) $ render "student/pages/enrollment"

renderCourse :: Course -> Splice AppHandler
renderCourse course = do
    groups <- lift $ Database.getGroupsByCourse (courseId course)
    I.runChildrenWith [
        ("courseName", courseName |- course)
      , ("groups",     I.mapSplices renderGroup groups)
      ]

renderGroup :: Group -> Splice AppHandler
renderGroup group =
    I.runChildrenWith [
        ("groupId",          groupId          |- group)
      , ("groupDescription", groupDescription |- group)
      ] 

------------------------------------------------------------------------------
handleEnrollment :: AppHandler ()
handleEnrollment = do
    sid      <- getStudentId
    mStudent <- Database.getStudent sid
    case mStudent of
      Nothing -> redirect "/404"
      Just _  -> do
        gid <- BS.unpack <$> fromMaybe "0" <$> getParam "groupId"
        now <- liftIO getCurrentTime
        _   <- Database.createEnrollment (gid, sid, now)
        redirect (BS.pack ("/student/" ++ sid))
