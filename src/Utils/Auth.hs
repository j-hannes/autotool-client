{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Utils.Auth
  ( getStudentId
  ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe            (fromMaybe)
------------------------------------------------------------------------------
import           Snap                  ((<$>), getParam)
------------------------------------------------------------------------------
import           Application
import           Model.Types

------------------------------------------------------------------------------
-- | Read the student id from query params.
getStudentId :: AppHandler StudentId
getStudentId = read <$> BS.unpack <$> fromMaybe "0" <$> getParam "studentId"
