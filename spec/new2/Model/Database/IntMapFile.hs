------------------------------------------------------------------------------
-- | A cascading IntMap database implementation
--
-- Each type is stored in an own IntMap data type which is written to an own
-- file. Supports full CRUD operations to create, restore, update and delete
-- objects from the Maps as well as a list operation.
--
module Model.Database.IntMapFile (
    -- ^ Tutor
    listTutors
  , createTutor
  , restoreTutor
  , updateTutor
  , deleteTutor

    -- ^ Course
  , createCourse

    -- ^ Task
  , createTask

    -- ^ Assignment
  , createAssignment
  ) where

import           Control.Applicative ((<$>))
import           Data.IntMap (IntMap, Key)
import qualified Data.IntMap as IM
import qualified System.IO.Strict as IOS

import           Model.Types.Assignment
import           Model.Types.Course
import           Model.Types.Task
import           Model.Types.Tutor


------------------------------------------------------------------------------
-- | type specific functions
------------------------------------------------------------------------------

listTutors :: IO [Tutor]
listTutors = list "tutor"

------------------------------------------------------------------------------
-- | Adds the tutor to the file regardless if a equal tutor already exists.
createTutor :: Tutor -> IO Key
createTutor = create "tutor"

restoreTutor :: Key -> IO (Maybe Tutor)
restoreTutor = restore "tutor"

updateTutor :: Key -> Tutor -> IO ()
updateTutor = update "tutor"

deleteTutor :: Key -> IO Bool
deleteTutor key = do
    courses <- listCourses -- forget it, we need functions like "get course by
                           -- tutor id, or even some kind of extra relation-
                           -- ship whatsoever
    tasks <- listTasks
    -- let course = IM.lookup
    if True
      then do
        db <- readDB "tutor" :: IO (IntMap Tutor)
        let db' = IM.delete key db
        writeDB db' "tutor"
        return True
      else return False

------------------------------------------------------------------------------
-- | Adds a course to the file but only if the related tutor can be found.
createCourse :: Course -> IO (Maybe Key)
createCourse course =
    with restoreTutor (courseTutor course) $ create "course" course


------------------------------------------------------------------------------
-- | Adds a task to the file but only if the related tutor can be found.
createTask :: Task -> IO (Maybe Key)
createTask task =
    with restoreTutor (taskTutor task) $ create "task" task

with :: (Key -> IO (Maybe a)) -> Key -> IO b -> IO (Maybe b)
with restoreFn key runFn = do
    obj <- restoreFn key
    case obj of
      Nothing -> return Nothing
      Just _ -> Just <$> runFn

createAssignment :: Assignment -> IO Key
createAssignment = create "assignment"


------------------------------------------------------------------------------
-- | generic functions
------------------------------------------------------------------------------

list :: (Read a) => DBFile -> IO [a]
list objectType = do
    db <- readDB objectType
    return $ IM.elems db

create :: (Read a, Show a) => DBFile -> a -> IO Key
create objectType obj = do
    db <- readDB objectType 
    let nextId = getNextId db
    let db' = IM.insert nextId obj db
    writeDB db' objectType
    return nextId

restore :: (Read a) => DBFile -> Key -> IO (Maybe a)
restore objectType key = do
    db <- readDB objectType
    return $ IM.lookup key db

update :: (Read a, Show a) => DBFile -> Key -> a -> IO ()
update objectType key obj = do
    db <- readDB objectType
    let db' = IM.update (\_ -> Just obj) key db
    writeDB db' objectType

{-
delete :: DBFile -> Key -> IO ()
delete objectType key = do
    db <- readDB objectType
    let db' = IM.delete key db
    writeDB db' objectType
-}

type DBFile = String

readDB :: (Read a) => DBFile -> IO (IntMap a)
readDB objectType = do
  contents <- IOS.readFile $ dbPath objectType
  return $ read contents

writeDB :: (Show a) => (IntMap a) -> DBFile -> IO ()
writeDB db objectType = writeFile (dbPath objectType) (show db)

dbPath :: DBFile -> String
dbPath objectType = "db/" ++ objectType ++ ".intmap"

getNextId :: IntMap a -> Key
getNextId m | IM.null m = 1
            | otherwise = maximum (IM.keys m) + 1
