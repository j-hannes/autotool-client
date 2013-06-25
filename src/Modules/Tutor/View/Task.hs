{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- |
module Modules.Tutor.View.Task
    ( bindTaskTreeSplice
    , bindFormSplices
    , taskTreeTemplate
    , taskFormTemplate
    ) where

------------------------------------------------------------------------------
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as BS
import           Data.List                      (intersperse)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Heist
import           Heist.Interpreted
import qualified Text.XmlHtml                   as X
------------------------------------------------------------------------------
import           Application
import           Autotool.XmlRpc.Types.TaskTree


------------------------------------------------------------------------------
-- | Type alias to bind (add) a splice to the heist state.
type SpliceBinder = HeistState AppHandler -> HeistState AppHandler


------------------------------------------------------------------------------
-- | The location of the template file in the /snaplets/heist/templates
-- folder.
taskTreeTemplate :: ByteString
taskTreeTemplate = "tutor/pages/select_task"


------------------------------------------------------------------------------
-- | The location of the template file in the /snaplets/heist/templates/forms
-- folder.
taskFormTemplate :: ByteString
taskFormTemplate = "tutor/forms/task_config"


------------------------------------------------------------------------------
-- | Binds a splice to the handlers heist state which renders a list of task
-- trees into a html template.
bindTaskTreeSplice :: [TaskTree] -> SpliceBinder
bindTaskTreeSplice = bindSplice "taskTrees" . mapSplices renderTaskTree


------------------------------------------------------------------------------
-- | Binds a splice to the handlers heist state which renders a list of task
-- trees into a html template.
bindFormSplices :: String
                -> String
                -> [(String, String)]
                -> Maybe String
                -> SpliceBinder 
bindFormSplices name desc doc err =
    bindSplices
      [ ("task-name",          textSplice $ T.pack name)
      , ("task-description",   taskDescriptionSplice desc)
      , ("task-documentation", taskDocumentationSplice doc)
      , ("verification-error", verificationErrorSplice err)
      ]


------------------------------------------------------------------------------
-- | Renders a error message into a bootstrap error window.
verificationErrorSplice :: Maybe String -> Splice AppHandler
verificationErrorSplice Nothing       = return [X.TextNode ""]
verificationErrorSplice (Just errMsg) = return [X.Element "pre" [
                                            ("class", "alert alert-warning")
                                          ] [
                                            X.TextNode $ T.pack errMsg
                                          ]]


------------------------------------------------------------------------------
-- | A splice that renders a list of tuples (linktext, url) into multiple 'a'
-- tags, spearated by a ' | ' separator.
taskDocumentationSplice :: [(String, String)] -> Splice AppHandler
taskDocumentationSplice linkList =
    return . intersperse separator $ map createLink linkList
  where
    separator = X.TextNode " | "


------------------------------------------------------------------------------
-- | Transform a tuple of a text and an url into an html link tag.
createLink :: (String, String) -> X.Node
createLink (text, url)= X.Element "a" attributes [linkTextNode]
  where
    attributes = [("href", T.pack url), ("target", "_blank")]
    linkTextNode = X.TextNode $ T.pack text


------------------------------------------------------------------------------
-- |
renderTaskTree :: TaskTree -> Splice AppHandler
renderTaskTree (Category name trees) = renderCategory name trees
renderTaskTree (Task name)           = renderTasklink name


------------------------------------------------------------------------------
-- |
renderCategory :: String -> [TaskTree] -> Splice AppHandler
renderCategory name taskTrees =
    runChildrenWith splices
  where
    splices =
      [ ("element",      singleTagNode "category")
      , ("categoryName", textSplice $ T.pack name)
      , ("subTrees",     mapSplices renderTaskTree taskTrees)]


------------------------------------------------------------------------------
-- |
renderTasklink :: String -> Splice AppHandler
renderTasklink name =
    runChildrenWith splices
  where
    splices =
      [ ("element",  singleTagNode "task")
      , ("taskName", textSplice $ T.pack name)]


------------------------------------------------------------------------------
-- |
singleTagNode :: Text -> Splice AppHandler
singleTagNode tag =  return [X.Element tag [] []]


------------------------------------------------------------------------------
-- | A splice that contains the task description.
taskDescriptionSplice :: String -> Splice AppHandler
taskDescriptionSplice = return . parseXML id
                     -- (map $ setCssClass $ T.pack cls)


------------------------------------------------------------------------------
-- | Parse a string into an XML forest with function application.
--   Take an inputString and parse it's content into an XML document. Then the
--   passed function gets applied to the content of the XML document to
--   possibly transform its content (just pass id in case of no
--   transformation). The resulting XML forest is returned in the end.
parseXML :: ([X.Node] -> [X.Node]) -> String -> [X.Node]
parseXML transformXml inputString =
    either textNode (transformXml . X.docContent) xmlDocument
  where
    xmlDocument = X.parseXML "" byteString
    byteString  = BS.pack inputString


------------------------------------------------------------------------------
-- | Takes a string and converts it to a list with a single TextNode in it.
textNode :: String -> [X.Node]
textNode txt = [X.TextNode . T.pack $ txt]
