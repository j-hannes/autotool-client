module Model.Adapter.File.Group where

import Control.Applicative ((<$>))
import Control.Monad (forM)

import Model.Adapter.File as SimpleStore
import Model.Adapter.File.Enrollment hiding (query)
import Model.Types.Enrollment
import Model.Types.Group

type CourseId = Integer

create :: CourseId -> String -> Int -> IO Group
create cid desc cap =
    SimpleStore.create "groups" group
  where
    group = Group 0 cid desc cap

getEnrollableGroups :: Integer -> IO [Group]
getEnrollableGroups uid = do
    enrollments <- restoreAll "enrollments"
    groups      <- restoreAll "groups"
    return $ filterEnrollableGroups uid enrollments groups

filterEnrollableGroups :: Integer -> [Enrollment] -> [Group] -> [Group]
filterEnrollableGroups uid enrollments groups =
    filter (not . belongsToEnrolledCourse) groups
  where
    belongsToEnrolledCourse group = groupCourseId group `elem` enrolledCourseIds
    enrolledCourseIds  = map groupCourseId enrolledGroups
    enrolledGroups     = filter isEnrolled groups
    isEnrolled group   = groupId group `elem` enrolledGroupIds
    belongsToStudent e = enrollmentStudentId e == uid

    enrolledGroupIds   = map enrollmentGroupId studentEnrollments
    studentEnrollments = filter belongsToStudent  enrollments

getAllGroups :: IO [Group]
getAllGroups = restoreAll "group"

getGroupById :: Integer -> IO Group
getGroupById gid = head <$> query groupId gid

getAllByStudentId :: Integer -> IO [Group]
getAllByStudentId sid = do
    enrollments <- getEnrollmentsByStudentId sid
    forM (map enrollmentGroupId enrollments) getGroupById 

query :: (Group -> Integer) -> Integer -> IO [Group]
query foreignId objectId = fmap (filter filterFn) (restoreAll "groups")
  where
    filterFn = \group -> foreignId group == objectId
