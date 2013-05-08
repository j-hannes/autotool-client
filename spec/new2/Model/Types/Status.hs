module Model.Types.Status where

data Status =
      Mandatory
    | Optional
    deriving (Eq, Read, Show)
