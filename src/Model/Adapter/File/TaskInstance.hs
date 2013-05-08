module Model.Adapter.File.TaskInstance where

import Control.Applicative ((<$>))

import Model.Adapter.File as SimpleStore
import Model.Types.TaskInstance

create :: Integer -> Integer -> String -> String -> String -> String
       -> IO TaskInstance
create tid uid desc sol doc sig =
    SimpleStore.create "taskInstances" taskInstance
  where
    taskInstance = TaskInstance 0 tid uid desc doc sol sig
    

getById :: Integer -> IO TaskInstance
getById tiid = head <$> query taskInstanceId tiid

getByTaskIdAndUserId :: Integer -> Integer
                                 -> IO (Maybe TaskInstance)
getByTaskIdAndUserId tid uid = do
    tasksByUserId <- query taskInstanceStudentId uid
    if null tasksByUserId
      then return Nothing
      else do
        let tasks = filter (\x -> taskInstanceTaskId x == tid) tasksByUserId
        if null tasks
          then return Nothing
          else return . Just $ head tasks

query :: (TaskInstance -> Integer) -> Integer -> IO [TaskInstance]
query foreignId objectId = fmap (filter filterFn) (restoreAll "taskInstances")
  where
    filterFn = \taskInstance -> foreignId taskInstance == objectId
