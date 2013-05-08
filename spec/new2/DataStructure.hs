-- Modellierungsvariante 1 - Objectbeziehungen
-- ===========================================
module DataStructure where

-- Diese Modellierungsvariante ergibt sich auf natürlichem Wege durch die
-- Abfragestruktur eines Programmes. Zum Beispiel soll eine Liste aller Kurse
-- des Tutors angezeigt werden. Die einfachste Möglichkeit, dies Umzusetzen
-- ist, den Datentyp Tutor mit eine Liste "seiner" Kurse als Attribut
-- auszustatten. Weiterhin sollen alle Gruppen und alle Aufgaben eines Kurses
-- angezeigt werden. Auch hier ist der effektivste Weg, dem DT Kurs
-- entsprechend eine Liste zugehörender Gruppen und zugewiesener Belege zu
-- geben.
--
-- Im Versuch der Datenmodellierung würde sich damit das folgende Datenmodell
-- ergeben:

-- import Data.Time (UTCTime)
type UTCTime = String -- simplification

-- LEVEL 1

data Tutor = Tutor
  { tutorName         :: Name
  , tutorCourses      :: [Course]
  , tutorTasks        :: [Task] -- L2
  }

data Course = Course
  { courseName        :: String
  , courseEnrollment  :: Timespan
  , courseExamAdmission  :: Int
  , courseGroups      :: [Group]
  , courseAssignments :: [Assignment] -- L2
  }

data Group = Group
  { groupName         :: String
  , groupCapacity     :: Int
  , groupEnrollments  :: [Enrollment]
  }

data Enrollment = Enrollment
  { enrollmentTime    :: UTCTime
  }

data Student = Student
  { studentName          :: Name
  , studentEnrollments   :: [Enrollment]
  , studentTaskInstances :: [TaskInstance] -- L2
  }

-- LEVEL 2

data Task = Task
  { taskName         :: String
  , taskType         :: String
  , taskScoringOrder :: ScoringOrder
  , taskSignature    :: String
  , taskAssignments  :: [Assignment]
  }

data Assignment = Assignment
  { assignmentStatus        :: Status
  , assignmentSubmission    :: Timespan
  , assignmentTaskInstances :: [TaskInstance]
  }

data TaskInstance = TaskInstance
  { taskInstanceSignature :: String
  , taskInstanceSolutions :: [Solution]
  }

data Solution = Solution
  { solutionContent   :: String
  , solutionScore     :: Int
  , solutionSize      :: Int
  , solutionTimestamp :: UTCTime
  }

-- LEVEL 3

data Name = Name
  { prename :: String
  , surname :: String
  }

data Timespan = Timespan
  { timespanStart :: UTCTime
  , timespanEnd   :: UTCTime
  }

data Status = Mandatory | Optional

data ScoringOrder = Increasing | Decreasing | None

-- Die Implementierung würde nun folgendermassen aussehen:
--
-- Es muessen nur eine Liste von Tutoren und eine Liste von Studenten
-- gespeichert werden, der rest is in denen enthalten:
--

tutors :: [Tutor]
tutors = [
    Tutor {
        tutorName = Name "Andreas" "Englisch"
      , tutorCourses = [
          Course {
              courseName = "Datenbanken I"
            , courseEnrollment = Timespan "01.04.2013" "10.04.2013"
            , courseExamAdmission = 50
            , courseGroups = [
                  Group {
                      groupName = "Montags und Donnerstags 17.15 Uhr"
                    , groupCapacity = 24
                    , groupEnrollments = []
                    }
                , Group {
                      groupName = "Dienstags 7.30 Uhr und Freitags 11.15 Uhr"
                    , groupCapacity = 24
                    , groupEnrollments = []
                    }
                ]
            , courseAssignments = [
                  Assignment {
                      assignmentStatus = Mandatory
                    , assignmentSubmission = Timespan "01.04.2013" "10.04.2013"
                    , assignmentTaskInstances = []
                    }
                ]
            }
        ]
      , tutorTasks = [
            Task {
                taskName = "Exp_Shortest_Missing-Direct-1"
              , taskType = "Exp_Shortest_Missing-Direct"
              , taskScoringOrder = Decreasing
              , taskSignature = "7f89sdg98e7gr...e9879"
              , taskAssignments = [
                    Assignment {
                        assignmentStatus = Mandatory
                      , assignmentSubmission = Timespan "01.04.2013" "10.04.2013"
                      , assignmentTaskInstances = []
                      }
                  ]
              }
          ]
      }
  ]

-- Hierbei ergeben sich bereits ein Problem: Der Beleg (Assignment) des Tutors
-- ist zweimal definiert, obwohl es sich um ein und dasselbe Objekt handelt.
-- Dieses Problem kann zunächst recht einfach umgangen werden, indem die
-- Definition des Beleg-Objektes in eine Wertzuweisung ausgelagert wird:

tutors' :: [Tutor]
tutors' = [
    Tutor {
        tutorName = Name "Andreas" "Englisch"
      , tutorCourses = [
          Course {
              courseName = "Datenbanken I"
            , courseEnrollment = Timespan "01.04.2013" "10.04.2013"
            , courseExamAdmission = 50
            , courseGroups = [
                  Group {
                      groupName = "Montags und Donnerstags 17.15 Uhr"
                    , groupCapacity = 24
                    , groupEnrollments = []
                    }
                , Group {
                      groupName = "Dienstags 7.30 Uhr und Freitags 11.15 Uhr"
                    , groupCapacity = 24
                    , groupEnrollments = []
                    }
                ]
            , courseAssignments = [assignment]
            }
        ]
      , tutorTasks = [
            Task {
                taskName = "Exp_Shortest_Missing-Direct-1"
              , taskType = "Exp_Shortest_Missing-Direct"
              , taskScoringOrder = Decreasing
              , taskSignature = "7f89sdg98e7gr...e9879"
              , taskAssignments = [assignment]
              }
          ]
      }
  ]
  where
    assignment =
        Assignment {
            assignmentStatus = Mandatory
          , assignmentSubmission = Timespan "01.04.2013" "10.04.2013"
          , assignmentTaskInstances = []
          }

-- Diese Art der Objektdefinition macht eigentlich auch für alle Objekte Sinn
-- und erhöht zudem die Übersichtlichkeit:

tutors'' :: [Tutor]
tutors'' = [tutor]
  where
    tutor = 
        Tutor {
            tutorName = Name "Andreas" "Englisch"
          , tutorCourses = [course]
          , tutorTasks = [task]
          }
    course = 
        Course {
            courseName = "Datenbanken I"
          , courseEnrollment = Timespan "15.03.2013" "22.03.2013"
          , courseExamAdmission = 50
          , courseGroups = [group1, group2]
          , courseAssignments = [assignment]
          }
    group1 =
        Group {
            groupName = "Montags und Donnerstags 17.15 Uhr"
          , groupCapacity = 24
          , groupEnrollments = []
          }
    group2 = 
        Group {
            groupName = "Dienstags 7.30 Uhr und Freitags 11.15 Uhr"
          , groupCapacity = 24
          , groupEnrollments = []
          }
    task = 
        Task {
            taskName = "Exp_Shortest_Missing-Direct-1"
          , taskType = "Exp_Shortest_Missing-Direct"
          , taskScoringOrder = Decreasing
          , taskSignature = "7f89sdg98e7gr...e9879"
          , taskAssignments = [assignment]
          }
    assignment =
        Assignment {
            assignmentStatus = Mandatory
          , assignmentSubmission = Timespan "01.04.2013" "10.04.2013"
          , assignmentTaskInstances = []
          }

-- DASHBOARD Andreas Englisch
--
-- Kurs "Datenbanken I"
-- --------------------
--   Einschreibung vorbei seit 22.03.2013
--   Eingeschriebene Studenten 0 von 48
--   Prüfungszulassung mit 50% bestandener Belegaufgaben
--   Kursstatus: [
--   

-- Nun können mit relativ einfachen Mitteln 

-- Trotzdem besteht bei diese Modellierung noch ein weiteres Problem: Die
-- eindeutige Identifizierung von Objekten. Da ein Objekt nun innerhalb
-- mehrerer anderer Objekte gespeichert werden kann, müssen alle Instanzen des
-- Objektes 

-- Betrachten wir eine komplexere Abfrage, und wie Aufwendig diese nun mit
-- Hilfe des Datanmodells umzusetzen ist:
--
-- Roter Bereich '
--
--
-- Studenten, die im Grünen Bereich sind

-- Durch diese Modellierungsvariante ergeben sich allerdings nun folgende
-- Probleme:
--
-- * A
-- * B
-- * C
