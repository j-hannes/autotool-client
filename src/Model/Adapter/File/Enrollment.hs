module Model.Adapter.File.Enrollment where

import Data.Time (UTCTime)

import qualified Model.Adapter.File as SimpleStore
import Model.Types.Enrollment

type GroupId = Integer
type StudentId = Integer

create :: GroupId -> StudentId -> UTCTime -> IO Enrollment
create gid sid time =
    SimpleStore.create "enrollments" enrollment
  where
    enrollment = Enrollment 0 gid sid time

getEnrollmentsByStudentId :: Integer -> IO [Enrollment]
getEnrollmentsByStudentId = query enrollmentStudentId
    
query :: (Enrollment -> Integer) -> Integer -> IO [Enrollment]
query foreignId objectId = fmap (filter filterFn) (SimpleStore.restoreAll "enrollments")
  where
    filterFn = \enrollment -> foreignId enrollment == objectId
