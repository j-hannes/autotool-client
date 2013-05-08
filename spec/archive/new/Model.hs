module Model
  (
    create
  , restore
  , update
  , delete
  , Storable
  ) where

{-
import           Model.Tutor          (Tutor)
import qualified Model.Tutor        as Tutor
import           Model.Student        (Student)
import qualified Model.Student      as Student
import           Model.Course         (Course)
import qualified Model.Course       as Course

import           Model.TaskTemplate   (TaskTemplate)
import qualified Model.TaskTemplate as TaskTemplate
import           Model.TaskConfig     (TaskConfig)
import qualified Model.TaskConfig   as TaskConfig
import           Model.Assignment     (Assignment)
import qualified Model.Assignment   as Assignment
import           Model.TaskInstance   (TaskInstance)
import qualified Model.TaskInstance as TaskInstance
import           Model.Solution       (Solution)
import qualified Model.Solution     as Solution
-}

{-
data OneToManyRelationship a b = OneToManyRelationship
  { one  :: a
  , many :: [b]
  } deriving (Eq, Read, Show)

data ManyToManyRelationship a b = ManyToManyRelationship
  {
  } deriving (Eq, Read, Show)

data Storable a = Storable
  { object     :: a
  , createdAt  :: UTCTime
  , createdBy  :: Int
  , modifiedAt :: UTCTime
  , modifiedBy :: Int
  } deriving (Eq, Read, Show)
-}

------------------------------------------------------------------------------

class Storable a where
  create  ::        a -> IO Int
  restore :: Int      -> IO (Maybe a)
  update  :: Int -> a -> IO Bool

------------------------------------------------------------------------------

{-
tutors :: IntMap (Storable Tutor)
tutors :: fromList []
-}
