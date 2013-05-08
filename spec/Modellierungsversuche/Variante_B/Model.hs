module Model where

-- import Data.Time (UTCTime)
type UTCTime  = String -- Fix me!
data Semester = SS13 deriving (Eq, Read, Show)

data Name = Name
  { namePrename :: String
  , nameSurname :: String
  } deriving (Eq, Read, Show)

------------------------------------------------------------------------------

data Timespan = Timespan
  { timespanStart :: UTCTime
  , timespanEnd   :: UTCTime
  } deriving (Eq, Read, Show)

data Tutor = Tutor
  { tutorName    :: Name
  , tutorTasks   :: [Task]
  , tutorCourses :: [Course]
  } deriving (Eq, Read, Show)

data Course = Course
  { courseName           :: String
  , courseSemester       :: Semester
  , courseEnrollmentTime :: Timespan
  , courseGroups         :: [Group]
  } deriving (Eq, Read, Show)

data Group = Group
  { groupName     :: String
  , groupCapacity :: Int
  , groupStudents :: [Student]
  } deriving (Eq, Read, Show)

data Student = Student
  { studentName          :: Name
  , studentNumber        :: String
  , studentPassword      :: String
  , studentEmail         :: String
  , studentGroups        :: [Group]
  , studentTaskInstances :: [TaskInstance]
  } deriving (Eq, Read, Show)

type Task         = String -- Fix me!
type TaskInstance = String -- Fix me!

------------------------------------------------------------------------------

dS :: [Tutor]
dS = [tutor]
  where
    tutor = Tutor {
        tutorName = Name "Peter" "Klöppel"
      , tutorTasks = []
      , tutorCourses = [course]
      }
    course = Course {
        courseName = "Datenbanken I"
      , courseSemester = SS13
      , courseEnrollmentTime = Timespan "01.04.2013" "15.04.2013"
      , courseGroups = [group_1, group_2]
      }
    group_1 = Group {
        groupName = "Montags und Donnerstags 17.15 Uhr"
      , groupCapacity = 22
      , groupStudents = []
      }
    group_2 = Group {
        groupName = "Dienstags 7.30 Uhr und Freitags 11.15 Uhr"
      , groupCapacity = 22
      , groupStudents = []
      }

type TutorZipper = (Maybe Tutor, ([Tutor], [Tutor]))

focusTutor :: Name -> [Tutor] -> TutorZipper
focusTutor name = findTutor (Nothing, ([], []))
  where
    findTutor zipper [] = zipper
    findTutor (_, (a, _)) (x:xs)
      | tutorName x == name = (Just x, (a, xs))
      | otherwise = findTutor (Nothing, (a ++ [x], [])) xs                          

--focusCourse :: TutorZipper -> String -> 
focusCourse tz@(Just tutor, (t1, t2)) = (course, ((c1, c2), tz))
  where course = findCourse tutor

--focusCourse :: Tutor -> String -> (CourseZipper, Rest)
--focusCourse [tutor]
