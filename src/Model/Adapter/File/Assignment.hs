module Model.Adapter.File.Assignment where

import Data.Time (UTCTime)

import qualified Model.Adapter.File as SimpleStore
import Model.Types.Assignment

type CourseId = Integer
type TaskId   = Integer

create :: CourseId -> TaskId -> Status -> UTCTime -> UTCTime -> IO Assignment
create cid tid sts start end =
    SimpleStore.create "assignments" assignment
  where
    assignment = Assignment 0 cid tid sts start end

countByTaskId :: Integer -> IO Int
countByTaskId = fmap length . query assignmentTaskId

getAllByCourseId :: Integer -> IO [Assignment]
getAllByCourseId = query assignmentCourseId
    
query :: (Assignment -> Integer) -> Integer -> IO [Assignment]
query foreignId objectId = fmap (filter filterFn) (SimpleStore.restoreAll "assignments")
  where
    filterFn = \assignment -> foreignId assignment == objectId
