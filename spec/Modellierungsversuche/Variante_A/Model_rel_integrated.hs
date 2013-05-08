module Model
  (
    -- ^ Core
    TaskType
  , TaskConfig
  , Assignment
  , TaskInstance
  , Solution
  
    -- ^ User
  , Tutor
  , Student

    -- ^ Organisation
  , Course

    -- ^ constructors
  , newTutor
  , newCourse

  ) where

import           Data.Time (UTCTime)


-- main data types

data TaskType = TaskType
    TaskName
    Documentation
    ExampleConfig
    [TaskConfig]
  deriving (Eq, Read, Show)

data TaskConfig = TaskConfig
    TaskType
    Content
    [Assignment]
  deriving (Eq, Read, Show)

data Assignment = Assignment
    TaskConfig
    SubmissionTime
    [TaskInstance]
  deriving (Eq, Read, Show)

data TaskInstance = TaskInstance
    Assignment
    TaskDescription
    Documentation
    ExampleSolution
    [Solution]
  deriving (Eq, Read, Show)

data Solution = Solution
    TaskInstance
    Content
    Score
  deriving (Eq, Read, Show)

data Tutor = Tutor
    PersonData
    [TaskConfig]
    [Course]
  deriving (Eq, Read, Show)

data Student = Student
    PersonData
    [Course]
    [TaskInstance]
  deriving (Eq, Read, Show)

data Course = Course
    Tutor
    Description
    Semester
    [Assignment]
    [Student]
  deriving (Eq, Read, Show)


-- additional data types

data PersonData = PersonData
    Name
    Id
  deriving (Eq, Read, Show)
    
data Name = Name
    String
    String
  deriving (Eq, Read, Show)

data Semester = SS13
  deriving (Eq, Read, Show)


-- doc types

type Content         = String
type Description     = String
type ExampleConfig   = String
type ExampleSolution = String
type Id              = String
type TaskDescription = String
type TaskName        = String
type Url             = String
type Score           = Int
type Documentation   = [(String, Url)]
type SubmissionTime  = (UTCTime, UTCTime)


-- constructors

newTutor :: String -> String -> String -> Tutor
newTutor prename surname identity =
    Tutor (PersonData (Name prename surname) identity) [] []

newCourse :: Tutor -> Description -> Semester -> (Tutor, Course)
newCourse tutor courseName semester = (tutor', course)
  where
    tutor' = Tutor personData taskConfigs (course : courses)
    (Tutor personData taskConfigs courses) = tutor
    course = Course tutor' courseName semester [] []

-- example application

main :: IO ()
main = do
    -- first we could create a tutor
    let tutor = newTutor "Johannes" "Waldmann" "J.W."

    -- we may want to keep an index of all tutors (later)
    let tutors = tutor : []

    -- now we let the tutor create a course
    -- the result is a new tutor and a new course
    let (tutor', course) = newCourse (head tutors) "Compilerbau" SS13

    -- we can add the course to our course index
    let courses = course : []

    -- but we need to modify our tutor index as well
    let tutors = tutor' : tail tutors

    -- so now if we ignore the indexes we created to instances, on of type
    -- tutor and one of type course
    --
    -- what we actually did was to create an infinite data structure (object)
    -- since tutor has a reference to the course and course has a reference to
    -- tutor
    --
    -- if we try to print these our output is infinite, but we still could work
    -- with those data types
    putStrLn $ show courses
    putStrLn $ show tutors

    -- further questions: can we extend our example a bit?
    -- how do the other CRUD funtions work here?
    -- can we put those structures in a RDB?
