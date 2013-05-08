module App where

import Data.Time (getCurrentTime, addUTCTime, NominalDiffTime)

import Autotool.Client (getInitialTaskConfig)
import Autotool.Client.Interface.CommandLine (pickTaskName,
  showTaskTemplateInfo, enterTaskConfig)

import Model

(...) :: (Show a) => String -> a -> IO ()
a ... b = putStrLn $ a ++ show b

oneWeek :: NominalDiffTime
oneWeek = 60 * 60 * 24 * 7

run :: IO ()
run = do
    -- (1) Create a tutor to manage the whole shit
    let tutor = Tutor (Name "Karsten" "Weicker")
    "Tutor is " ... tutor
    tutorId <- createTutor tutor
    "Tutor created with ID #" ... tutorId

    -- (2) Create a first course
    time <- getCurrentTime
    let enrollStart    = time
        enrollEnd      = addUTCTime oneWeek enrollStart
        enrollmentTime = Timespan enrollStart enrollEnd
        nameOfCourse   = "Algorithmen und Datenstrukturen"
        course         = Course tutorId nameOfCourse SS13 enrollmentTime
    "Course is " ... course
    courseId <- createCourse course
    "Course created with ID #" ... courseId

    -- (3) Configure a task (Autotool interaction)
    taskName <- pickTaskName    
    (sampleConfig, configDoc, scoringOrder) <- getInitialTaskConfig taskName
    showTaskTemplateInfo sampleConfig configDoc scoringOrder
    configSig <- enterTaskConfig taskName sampleConfig
    let task = Task tutorId "My first task" taskName scoringOrder configSig
    "Task is " ... task
    taskId <- createTask task
    "Task created with ID #" ... taskId

    -- (4) Assign the task to the course
    let submissionStart = enrollEnd
        submissionEnd   = addUTCTime oneWeek submissionStart
        submissionTime  = Timespan submissionStart submissionEnd
        assignment      = Assignment taskId courseId Mandatory submissionTime
    "Assignment is " ... assignment
    assignmentId <- createAssignment assignment
    ("Task #" ++ (show taskId) ++ " assigned to Course #") ... courseId
    
    -- thats it from the tutor's side
    putStrLn "---------"

    -- (5) Now
    
