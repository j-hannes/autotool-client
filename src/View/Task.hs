{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- |
module View.Task
    ( bindTaskTreeSplice
    , taskTreeTemplate
    ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as T
import           Heist
import           Heist.Interpreted
import qualified Text.XmlHtml as X
------------------------------------------------------------------------------
import           Application
import           Autotool.Client.Types.TaskTree


------------------------------------------------------------------------------
-- | Type alias to bind (add) a splice to the heist state.
type SpliceBinder = HeistState AppHandler -> HeistState AppHandler


------------------------------------------------------------------------------
-- | The location of the template file in the /snaplets/heist/templates
-- folder.
taskTreeTemplate :: ByteString
taskTreeTemplate = "select_task"


------------------------------------------------------------------------------
-- | Binds a splice to the handlers heist state which renders a list of task
-- trees into a html template.
bindTaskTreeSplice :: [TaskTree] -> SpliceBinder
bindTaskTreeSplice = bindSplice "taskTrees" . mapSplices renderTaskTree


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
      [ ("element",      categorySplice)  -- singleTagNode "category")
      , ("categoryName", textSplice $ T.pack name)
      , ("subTrees",     mapSplices renderTaskTree taskTrees)]

categorySplice = return [
      X.Element "div" [("class", "tag")] [
        X.Element "span" [("class", "category")] [
          X.Element "categoryName" [] []
        ]
      , X.Element "ul" [] [
          X.Element "subTrees" [] [
            X.Element "li" [] [
              X.Element "element" [] []
            ]
          ]
        ]
      ]
    ]


------------------------------------------------------------------------------
-- |
renderTasklink :: String -> Splice AppHandler
renderTasklink name =
    runChildrenWith splices
  where
    splices =
      [ ("element",  taskSplice)  -- singleTagNode "task")
      , ("taskName", textSplice $ T.pack name)]

taskSplice = return [
      X.Element "div" [("class", "ta")] [
        X.Element "a" [("href", "/task/configure/${taskName}")] [
          X.Element "taskName" [] []
        ]
      ]
    ]


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
