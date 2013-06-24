{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Utils.Auth
  ( getStudentId
  , getTutorId
  ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe            (fromMaybe)
------------------------------------------------------------------------------
import           Snap                  ((<$>), getParam)
------------------------------------------------------------------------------
import           Application
import           Model.Datatypes

------------------------------------------------------------------------------
-- | Read the student id from query params.
getStudentId :: AppHandler StudentId
getStudentId = BS.unpack <$> fromMaybe "" <$> getParam "studentId"

------------------------------------------------------------------------------
-- | Read the tutor id from query params.
getTutorId :: AppHandler TutorId
getTutorId = BS.unpack <$> fromMaybe "0" <$> getParam "tutorId"
