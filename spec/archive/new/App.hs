module App
  ( run
  ) where

import Model.File
import Types.Tutor

run :: IO ()
run = do
  let tutor = newTutor "Fritz" "Egner"
  create tutor
  return ()
