Data Types
==========

* *Tutor*
  - name           :: Name

* *Course*
  + tutor          :: Int
  - name           :: String
  - semester       :: Semester
  - enrollmentTime :: Timespan

* *Group*
  + course         :: Int
  - name           :: String
  - capacity       :: Int

* *Enrolment*
  + group          :: Int
  + student        :: Int
  - timestamp      :: UTCTime

* *Student*
  - name           :: Name
  - number         :: String
  - password       :: String
  - email          :: String

* *Task*
  + tutor          :: Int
  - name           :: String
  - taskName       :: String
  - scoringOrder   :: ScoringOrder
  - signature      :: String

* *Assignment*
  + task           :: Int
  + course         :: Int
  - status         :: Status
  - submissionTime :: Timespan

* *TaskInstance*
  + student        :: Int
  + assignment     :: Int
  - signature      :: String

* *Solution*
  + taskInstance   :: Int
  - content        :: String
  - score          :: Int
  - size           :: Int
  - timestamp      :: UTCTime

* Name
  - prename :: String
  - surname :: String

* Timespan
  - start   :: UTCTime
  - end     :: UTCTime


Functions
=========

highscore :: Assignment -> Maybe Int
highscore assignment
   | order == None       = Nothing
   | order == Increasing = (Just . maximum) scores
   | order == Decreasing = (Just . minimum) scores
  where
   orde   = (order . taskType . taskConfig)         assignment 
   scores = (map score . map solutions . taskInstances) assignment

numberOfSubmissions :: Assignment -> Int
numberOfSubmissions = sum . numberOf . taskInstances
  
bestScore :: Assignment -> Maybe Int
bestScore assignment = map    taskInstances

Helper
------

numberOf = length
