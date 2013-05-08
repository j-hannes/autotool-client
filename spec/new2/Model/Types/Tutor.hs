module Model.Types.Tutor where

import Model.Types.Name

data Tutor = Tutor
  { tutorName :: Name
  } deriving (Eq, Show, Read)
