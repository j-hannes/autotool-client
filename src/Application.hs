{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Data.IORef                (IORef)
import           Data.Map                  (Map)
import           Data.SafeCopy             (deriveSafeCopy, base)
import           Data.Typeable             (Typeable)
import           Control.Lens
------------------------------------------------------------------------------
import           Snap
import           Snap.Snaplet.AcidState    (Acid, makeAcidic)
import           Snap.Snaplet.AcidState    (HasAcid(getAcidStore))
import           Snap.Snaplet.Heist
import           Snap.Snaplet.SqliteSimple hiding (Query)
------------------------------------------------------------------------------
import           Model.Types



------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)

    , _assignments        :: IORef (Map Integer Assignment)
    , _courses            :: IORef (Map Integer Course)
    , _enrollments        :: IORef (Map Integer Enrollment)
    , _groups             :: IORef (Map Integer Group)
    , _solutions          :: IORef (Map Integer Solution)
    , _taskInstances      :: IORef (Map Integer TaskInstance)
    , _tasks              :: IORef (Map Integer Task)
    , _tutors             :: IORef (Map Integer Tutor)
    , _students           :: IORef (Map Integer Student)

    , _db                 :: Snaplet Sqlite

    , _acid_tutors        :: Snaplet (Acid TutorStore)
    , _acid_students      :: Snaplet (Acid StudentStore)
    , _acid_courses       :: Snaplet (Acid CourseStore)
    , _acid_groups        :: Snaplet (Acid GroupStore)
    , _acid_enrollments   :: Snaplet (Acid EnrollmentStore)
    , _acid_tasks         :: Snaplet (Acid TaskStore)
    , _acid_assignments   :: Snaplet (Acid AssignmentStore)
    , _acid_taskInstances :: Snaplet (Acid TaskInstanceStore)
    , _acid_solutions     :: Snaplet (Acid SolutionStore)
    
    }

newtype AssignmentStore = AssignmentStore {
    _assignmentStore :: Map Integer Assignment
  } deriving (Eq, Read, Show, Typeable)

newtype CourseStore = CourseStore {
    _courseStore :: Map Integer Course
  } deriving (Eq, Read, Show, Typeable)

newtype EnrollmentStore = EnrollmentStore {
    _enrollmentStore :: Map Integer Enrollment
  } deriving (Eq, Read, Show, Typeable)

newtype GroupStore = GroupStore {
    _groupStore :: Map Integer Group
  } deriving (Eq, Read, Show, Typeable)

newtype SolutionStore = SolutionStore {
    _solutionStore :: Map Integer Solution
  } deriving (Eq, Read, Show, Typeable)

newtype TaskStore = TaskStore {
    _taskStore :: Map Integer Task
  } deriving (Eq, Read, Show, Typeable)

newtype TaskInstanceStore = TaskInstanceStore {
    _taskInstanceStore :: Map Integer TaskInstance
  } deriving (Eq, Read, Show, Typeable)

newtype TutorStore = TutorStore {
    _tutorStore :: Map Integer Tutor
  } deriving (Eq, Read, Show, Typeable)

newtype StudentStore = StudentStore {
    _studentStore :: Map Integer Student
  } deriving (Eq, Read, Show, Typeable)

makeLenses ''AssignmentStore
makeLenses ''CourseStore
makeLenses ''EnrollmentStore
makeLenses ''GroupStore
makeLenses ''SolutionStore
makeLenses ''TaskStore
makeLenses ''TaskInstanceStore
makeLenses ''TutorStore
makeLenses ''StudentStore

deriveSafeCopy 0 'base ''AssignmentStore
deriveSafeCopy 0 'base ''CourseStore
deriveSafeCopy 0 'base ''EnrollmentStore
deriveSafeCopy 0 'base ''GroupStore
deriveSafeCopy 0 'base ''SolutionStore
deriveSafeCopy 0 'base ''TaskStore
deriveSafeCopy 0 'base ''TaskInstanceStore
deriveSafeCopy 0 'base ''TutorStore
deriveSafeCopy 0 'base ''StudentStore

makeAcidic ''CourseStore []
makeAcidic ''EnrollmentStore []
makeAcidic ''GroupStore []
makeAcidic ''SolutionStore []
makeAcidic ''TaskStore []
makeAcidic ''TaskInstanceStore []
makeAcidic ''TutorStore []
makeAcidic ''StudentStore []

makeLenses ''App

instance HasAcid App AssignmentStore where
    getAcidStore  = view (acid_assignments . snapletValue)

-- instance HasAcid App CourseStore where
    -- getAcidStore  = view (acid_courses . snapletValue)


instance HasHeist App where
    heistLens = subSnaplet heist

instance HasSqlite (Handler b App) where
   getSqliteState = with db get

------------------------------------------------------------------------------
type AppHandler = Handler App App
