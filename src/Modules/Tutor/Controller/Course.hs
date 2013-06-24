{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for creating and managing courses.
module Modules.Tutor.Controller.Course
  ( handleCourseForm
  ) where

------------------------------------------------------------------------------
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time
import           System.Locale
------------------------------------------------------------------------------
import           Snap
import           Text.Digestive.Form (Form, (.:), check, stringRead, text)
import           Text.Digestive.Snap
------------------------------------------------------------------------------
import           Application
import qualified Database.Switch     as Database
import           Utils.Form


------------------------------------------------------------------------------
-- | Render a form to display a course.
handleCourseForm :: AppHandler ()
handleCourseForm = do
    time <- convertToText <$> liftIO getCurrentTime
    (view, courseData) <- runForm "form" (courseForm time)
    maybe (renderForm "tutor/forms/course" view) createCourse courseData


------------------------------------------------------------------------------
-- | Format the time into the format used in the format. (This should go into
-- a central place.
convertToText :: UTCTime -> Text
convertToText = T.pack . formatTime defaultTimeLocale "%d/%m/%Y %H:%M:%S"


------------------------------------------------------------------------------
-- | Data required in the course form.
data CourseFormData = CourseFormData
  { formCourseName          :: Text
  , formEnrollmentOpening   :: Text
  , formEnrollmentDeadline  :: Text
  , formPassCriteria        :: Double
  , group1Name              :: Text
  , group1Capacity          :: Int
  , group2Name              :: Text
  , group2Capacity          :: Int
  } deriving (Show)


------------------------------------------------------------------------------
-- | Transform the Create a new course from the entered data.
--
-- TODO Submit a user information message into the session that can then be
-- displayed on the main page instead of displaying a separate page.
createCourse :: CourseFormData -> AppHandler ()
createCourse cfd = do
    cid <- Database.createCourse (tid, name, sem, enrStart, enrEnd, pc)
    -- liftIO $ print cid
    _   <- Database.createGroup  (cid, g1n, g1c)
    _   <- Database.createGroup  (cid, g2n, g2c)
    redirect "/tutor"
  where
    tid      = "51c8235ef4d13fc80f76c462"
    name     = T.unpack    $ formCourseName         cfd
    sem      = "SS13"
    enrStart = convertDate $ formEnrollmentOpening  cfd
    enrEnd   = convertDate $ formEnrollmentDeadline cfd
    pc       =               formPassCriteria       cfd
    g1n      = T.unpack    $ group1Name             cfd
    g1c      =               group1Capacity         cfd
    g2n      = T.unpack    $ group2Name             cfd
    g2c      =               group2Capacity         cfd


------------------------------------------------------------------------------
-- | Digestive course form.
courseForm :: Text -> Form Text AppHandler CourseFormData
courseForm time = CourseFormData
    <$> "courseName" .: check coursenameEmptyMsg notEmpty (text Nothing)
    <*> "enrollmentOpening"   .: (text $ Just time)
    <*> "enrollmentDeadline"  .: (text Nothing)
    <*> "passCriteria"        .: check assignInvalidMsg isPercent
                                 (stringRead "incorrect number" $ Just 0)
    <*> "group1Name"          .: (text Nothing)
    <*> "group1Capacity"      .: (stringRead "incorrect number" Nothing)
    <*> "group2Name"          .: (text Nothing)
    <*> "group2Capacity"      .: (stringRead "incorrect number" Nothing)


------------------------------------------------------------------------------
-- | Check against a valid percentage.
isPercent :: Double -> Bool
isPercent x = x <= 100


------------------------------------------------------------------------------
-- | Error message for missing course name.
coursenameEmptyMsg :: Text
coursenameEmptyMsg = "bitte Kursnamen eingeben"


------------------------------------------------------------------------------
-- | Error message for invalid assignment number input.
assignInvalidMsg :: Text
assignInvalidMsg = "ungültige Prozentangabe"
