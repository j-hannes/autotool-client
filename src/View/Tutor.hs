{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- |
module View.Tutor
    ( bindTaskConfigListSplice
    ) where

------------------------------------------------------------------------------
-- import           Data.ByteString                (ByteString)
-- import qualified Data.ByteString.Char8          as BS
-- import           Data.List                      (intersperse)
-- import           Data.Text                      (Text)
import           Data.Maybe                     (fromJust)
import qualified Data.Text                      as T
import           Heist
import           Heist.Interpreted
-- import qualified Text.XmlHtml                   as X
------------------------------------------------------------------------------
import           Application
import           Model.Types.TaskConfig         (TaskConfig)
import qualified Model.Types.TaskConfig         as TaskConfig
-- import           Autotool.Client.Types.TaskTree


------------------------------------------------------------------------------
-- | Type alias to bind (add) a splice to the heist state.
type SpliceBinder = HeistState AppHandler -> HeistState AppHandler


------------------------------------------------------------------------------
-- | Binds a splice to the handlers heist state which renders a list of task
-- trees into a html template.
bindTaskConfigListSplice :: [TaskConfig] -> SpliceBinder
bindTaskConfigListSplice = bindSplice "taskConfigs" . 



