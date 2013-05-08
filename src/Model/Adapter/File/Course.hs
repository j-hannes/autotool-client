module Model.Adapter.File.Course where

import Control.Applicative ((<$>))
import Data.Time (UTCTime)

import Model.Adapter.File as SimpleStore
import Model.Adapter.File.Group (getGroupById)
import Model.Types.Group
import Model.Types.Course

type TutorId = Integer

create :: TutorId -> String -> String -> Maybe UTCTime -> Maybe UTCTime
       -> Double -> IO Course
create tid name sem enrStart enrEnd pass =
    SimpleStore.create "courses" course
  where
    course = Course 0 tid name sem enrStart enrEnd pass

-- PRIMARY KEY
getById :: Integer -> IO Course
getById sid = head <$> query courseId sid

-- FOREIGN KEY (own attribute)
getAllByTutorId :: Integer -> IO [Course]
getAllByTutorId = query courseTutorId

-- FOREIGN KEY (foreign attribute)
getCourseByGroupId :: Integer -> IO Course
getCourseByGroupId gid = do
    group <- getGroupById gid
    getById (groupCourseId group)
    

query :: (Course -> Integer) -> Integer -> IO [Course]
query foreignId objectId = fmap (filter filterFn) (restoreAll "courses")
  where
    filterFn = \course -> foreignId course == objectId
