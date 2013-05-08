module Model where

import Data.Set
import Types ( Tutor, Course, Group, Student, Task, Assignment, TaskInstance
             , Solution )


-------------------------------------------------------------------------------
-- INDICES
-------------------------------------------------------------------------------

newtype TutorId = TutorId
  { unTutorId :: Integer
  } deriving (Eq, Ord, Read, Show)

newtype CourseId = CourseId
  { unCourseId :: Integer
  } deriving (Eq, Ord, Read, Show)

newtype GroupId = GroupId
  { unGroupId   :: Integer
  } deriving (Eq, Ord, Read, Show)

newtype StudentId = StudentId
  { unStudentId :: Integer
  } deriving (Eq, Ord, Read, Show)

newtype TaskId = TaskId
  { unTaskId :: Integer
  } deriving (Eq, Ord, Read, Show)

newtype AssignmentId = AssignmentId
  { unAssignmentId :: Integer
  } deriving (Eq, Ord, Read, Show)

newtype TaskInstanceId = TaskInstanceId
  { unTaskInstanceId :: Integer
  } deriving (Eq, Ord, Read, Show)

newtype SolutionId = SolutionId
  { unSolutionId :: Integer
  } deriving (Eq, Ord, Read, Show)


-------------------------------------------------------------------------------
-- TUTOR
-------------------------------------------------------------------------------

data TutorModel = TutorModel {
    tutorId      :: TutorId
  , tutorData    :: Tutor
  , tutorCourses :: Set CourseId
  , tutorTasks   :: Set TaskId
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- COURSE
-------------------------------------------------------------------------------

data CourseModel = CourseModel {
    courseId          :: CourseId
  , courseData        :: Course
  , courseTutor       :: TutorId
  , courseGroups      :: Set GroupId
  , courseAssignments :: Set AssignmentId
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- GROUP
-------------------------------------------------------------------------------

data GroupModel = GroupModel {
    groupId       :: GroupId
  , groupData     :: Group
  , groupCourse   :: CourseId
  , groupStudents :: Set StudentId
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- STUDENT
-------------------------------------------------------------------------------

data StudentModel = StudentModel {
    studentId            :: StudentId
  , studentData          :: Student
  , studentGroups        :: Set GroupId
  , studentTaskInstances :: Set TaskInstanceId
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- TASK
-------------------------------------------------------------------------------

data TaskModel = TaskModel {
    taskId          :: TaskId
  , taskData        :: Task
  , taskTutor       :: TutorId
  , taskAssignments :: Set AssignmentId
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- ASSIGNMENT
-------------------------------------------------------------------------------

data AssignmentModel = AssignmentModel {
    assignmentId            :: AssignmentId
  , assignmentData          :: Assignment
  , assignmentTask          :: TaskId
  , assignmentTaskInstances :: Set TaskInstanceId
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- TASKINSTANCE
-------------------------------------------------------------------------------

data TaskInstanceModel = TaskInstanceModel {
    taskInstanceId         :: TaskInstanceId
  , taskInstanceData       :: TaskInstance
  , taskInstanceStudent    :: StudentId
  , taskInstanceAssignment :: Set AssignmentId
  , taskInstanceSolutions  :: Set SolutionId
  } deriving (Eq, Read, Show)


-------------------------------------------------------------------------------
-- SOLUTION
-------------------------------------------------------------------------------

data SolutionModel = SolutionModel {
    solutionId           :: SolutionId
  , solutionData         :: Solution
  , solutionTaskInstance :: TaskInstanceId
  } deriving (Eq, Read, Show)
