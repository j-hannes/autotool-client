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
  { tutorName            :: Name
  , tutorCourses         :: [Course]
  , tutorTasks           :: [Task] -- L2
  }

data Course = Course
  { courseName           :: String
  , courseEnrollment     :: Timespan
  , courseExamAdmission  :: Int
  , courseGroups         :: [Group]
  , courseAssignments    :: [Assignment] -- L2
  }

data Group = Group
  { groupName            :: String
  , groupCapacity        :: Int
  , groupStudents        :: [Student]
  }

data Student = Student
  { studentName          :: Name
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
  { solutionContent    :: String
  , solutionEvaluation :: String
  , solutionResult     :: Maybe Result
  , solutionTimestamp  :: UTCTime
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

data Result = Result
  { resultScore :: Int
  , resultSize  :: Int
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
                    , groupCapacity = 3
                    , groupStudents = [{- ... -}]
                    }
                , Group {
                      groupName = "Dienstags 7.30 Uhr und Freitags 11.15 Uhr"
                    , groupCapacity = 3
                    , groupStudents = [{- ... -}]
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
                    , groupStudents = [{- ... -}]
                    }
                , Group {
                      groupName = "Dienstags 7.30 Uhr und Freitags 11.15 Uhr"
                    , groupCapacity = 24
                    , groupStudents = [{- ... -}]
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
          , groupStudents = [s1, s2, s5]
          }
    group2 = 
        Group {
            groupName = "Dienstags 7.30 Uhr und Freitags 11.15 Uhr"
          , groupCapacity = 24
          , groupStudents = [s3, s4]
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
    s1 =
        Student {
            studentName = Name "Ab" "Mn"
          , studentTaskInstances = [ti1]
          }
    s2 =
        Student {
            studentName = Name "Bc" "No"
          , studentTaskInstances = [ti2]
          }
    s3 =
        Student {
            studentName = Name "Cd" "Op"
          , studentTaskInstances = [ti3]
          }
    s4 =
        Student {
            studentName = Name "De" "Pq"
          , studentTaskInstances = [ti4]
          }
    s5 =
        Student {
            studentName = Name "Ef" "Qr"
          , studentTaskInstances = [ti5]
          }
    ti1 =
        TaskInstance {
            taskInstanceSignature = "9fe80few98f09ew"
          , taskInstanceSolutions = [so1]
          }
    ti2 =
        TaskInstance {
            taskInstanceSignature = "j6i54j5oi4jo3"
          , taskInstanceSolutions = []
          }
    ti3 =
        TaskInstance {
            taskInstanceSignature = "8697dfa65s9fd"
          , taskInstanceSolutions = []
          }
    ti4 =
        TaskInstance {
            taskInstanceSignature = "ji3j4oi2j4oi4583"
          , taskInstanceSolutions = []
          }
    ti5 =
        TaskInstance {
            taskInstanceSignature = "jijoi43j53oi4koir98w"
          , taskInstanceSolutions = []
          }
    so1 =
        Solution {
            solutionContent = "3"
          , solutionEvaluation = "Okay."
          , solutionResult = Just (Result 10 1)
          , solutionTimestamp = "03.04.2013"
          }


-- DASHBOARD Dietmar Reimann - Sommersemester 2013
-- ===============================================
--
-- Kurs "C Programmierung"
-- -----------------------
--   Einschreibung vorbei seit 22.03.2013
--   Prüfungszulassung mit 50% bestandener Belegaufgaben
--   Zulassungsquote derzeit 72%
--
--   Übungsgruppen
--   -------------
--     Montags und Donnerstags 17.15 Uhr
--     ---------------------------------
--       Eingeschriebene Studenten 2 von 3
--       
--     Dienstags 7.30 Uhr und Freitags 11.15 Uhr
--     -----------------------------------------
--       Eingeschriebene Studenten 3 von 3
--
--   Beste Studenten
--   ---------------
--     Peter Müller: 42 HP
--     Franz Joseph: 23 HP
--     Thomas Mann:  20 HP
--
--   Belege
--   ------
--     Palindrom-Direct-2
--     ------------------
--       Zusatzaufgabe
--       Abgabe vorbei seit 21.03.2013
--       Einsendungen 124 (davon erfolgreich 58)
--       Beste Bewertung 12 (absteigende Bewertungsreihenfolge)
--       62% der Studenten haben die Aufgabe richtig gelöst
--
--     Hanoi-Quiz-54
--     -------------
--       Pflichtaufgabe
--       Abgabe noch bis 01.04.2013
--       Einsendungen 87 (davon erfolgreich 14)
--       Beste Bewertung 76 (aufsteigende Bewertungsreihenfolge)
--       34% der Studenten haben die Aufgabe richtig gelöst
--    
--     Exp_Shortest_Missing-Direct-1
--     -----------------------------
--       Pflichtaufgabe
--       Abgabe startet 10.04.2013

-- DASHBOARD Franz Joseph - Sommersemester 2013
-- ============================================
--
-- Kurs "C Programmierung" (Dietmar Reimann)
-- -----------------------------------------
--   Prüfungszulassung mit 50% bestandener Belegaufgaben
--   Bisher 2 von 3 Aufgaben richtig gelöst (das sind 66%)
--
--   Übungsgruppe
--   ------------
--     Dienstags 7.30 Uhr und Freitags 11.15 Uhr
--
--   Belege
--   ------
--     Palindrom-Direct-2
--     ------------------
--       Zusatzaufgabe
--       Abgabe vorbei seit 21.03.2013
--       Gesamt-Wertungen (ok(4), no(17))
--       Beste Bewertung 12 (absteigende Bewertungsreihenfolge)
--       Meine Beste Bewertung 12
--
--     Hanoi-Quiz-54
--     -------------
--       Pflichtaufgabe
--       Abgabe noch bis 01.04.2013
--       Gesamt-Wertungen (ok(1), no(11))
--       Beste Bewertung 76 (aufsteigende Bewertungsreihenfolge)
--       Meine Beste Bewertung 52
--       [Aufgabe lösen]

-- Nun können mit relativ einfachen Mitteln 

-- Trotzdem besteht bei diese Modellierung noch ein weiteres Problem: Die
-- eindeutige Identifizierung von Objekten. Da ein Objekt nun innerhalb
-- mehrerer anderer Objekte gespeichert werden kann, müssen alle Instanzen des
-- Objektes 

-- Betrachten wir eine komplexere Abfrage, und wie Aufwendig diese nun mit
-- Hilfe des Datanmodells umzusetzen ist:
--
-- Roter Bereich  = Studenten, die die Zulassung nicht mehr erhalten können
-- Orangener Bereich = Studenten, die alle weiteren Belege erfüllen müssen,
--                                um die Zulassung noch zu erhalten
-- Gelber Bereich = Studenten, die unterhalb der Zulassungsrate liegen
-- Grüner Bereich = Studenten, die oberhalb oder auf der Zulassungsrate liegen
-- Blauer Bereich = Studenten, die Zulassung bereits geschafft haben
--
-- Das setzt voraus, dass die Anzahl aller Aufgaben bereits bekannt ist ...
-- das ist jedoch nicht immer der Fall! Also:
--
-- Grüner Bereich = Oberhalb oder gleich der Zulassungsrate
-- Roter Bereich = Unterhalb der Zulassungsrate
--
-- Studenten, die im Grünen Bereich sind

-- Durch diese Modellierungsvariante ergeben sich allerdings nun folgende
-- Probleme:
--
-- * A
-- * B
-- * C
