{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- |
module View.Solution
    ( bindFormSplices
    , solutionFormTemplate
    ) where

------------------------------------------------------------------------------
import           Data.ByteString                (ByteString)
-- import qualified Data.ByteString.Char8          as BS
import           Data.List                      (intersperse)
import qualified Data.Text                      as T
import           Heist
import           Heist.Interpreted
import qualified Text.XmlHtml                   as X
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Type alias to bind (add) a splice to the heist state.
type SpliceBinder = HeistState AppHandler -> HeistState AppHandler


------------------------------------------------------------------------------
-- | The location of the template file in the /snaplets/heist/templates/forms
-- folder.
solutionFormTemplate :: ByteString
solutionFormTemplate = "student/forms/create_solution"


------------------------------------------------------------------------------
-- | Binds a splice to the handlers heist state which renders a list of solution
-- trees into a html template.
bindFormSplices :: String
                -> [(String, String)]
                -> Maybe String
                -> Maybe String
                -> SpliceBinder 
bindFormSplices taskDescription doc eva err =
    bindSplices
      [ ("taskDescription",   textSplice $ T.pack taskDescription)
      , ("taskDocumentation", taskDocumentationSplice doc)
      , ("verificationError", verificationErrorSplice err)
      , ("evaluation",        evaluationSplice eva)
      ]


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
-- | Renders a error message into a bootstrap error window.
verificationErrorSplice :: Maybe String -> Splice AppHandler
verificationErrorSplice Nothing       = return [X.TextNode ""]
verificationErrorSplice (Just errMsg) = return [X.Element "pre" [
                                            ("class", "alert alert-warning")
                                          ] [
                                            X.TextNode $ T.pack errMsg
                                          ]]


------------------------------------------------------------------------------
-- | Renders a error message into a bootstrap error window.
evaluationSplice :: Maybe String -> Splice AppHandler
evaluationSplice Nothing           = return [X.TextNode ""]
evaluationSplice (Just evaluation) = return [X.Element "pre" [
                                            ("class", "alert alert-success")
                                          ] [
                                            X.TextNode $ T.pack evaluation
                                          ]]


------------------------------------------------------------------------------
-- | A splice that renders a list of tuples (linktext, url) into multiple 'a'
-- tags, spearated by a ' | ' separator.
{-
solutionDocumentationSplice :: [(String, String)] -> Splice AppHandler
solutionDocumentationSplice linkList =
    return . intersperse separator $ map createLink linkList
  where
    separator = X.TextNode " | "
-}


------------------------------------------------------------------------------
-- | Transform a tuple of a text and an url into an html link tag.
{-
createLink :: (String, String) -> X.Node
createLink (text, url)= X.Element "a" attributes [linkTextNode]
  where
    attributes = [("href", T.pack url), ("target", "_blank")]
    linkTextNode = X.TextNode $ T.pack text
-}


------------------------------------------------------------------------------
-- | A splice that contains the solution description.
{-
solutionDescriptionSplice :: String -> Splice AppHandler
solutionDescriptionSplice = return . parseXML id
-}


------------------------------------------------------------------------------
-- | Parse a string into an XML forest with function application.
--   Take an inputString and parse it's content into an XML document. Then the
--   passed function gets applied to the content of the XML document to
--   possibly transform its content (just pass id in case of no
--   transformation). The resulting XML forest is returned in the end.
{-
parseXML :: ([X.Node] -> [X.Node]) -> String -> [X.Node]
parseXML transformXml inputString =
    either textNode (transformXml . X.docContent) xmlDocument
  where
    xmlDocument = X.parseXML "" byteString
    byteString  = BS.pack inputString
-}


------------------------------------------------------------------------------
-- | Takes a string and converts it to a list with a single TextNode in it.
{-
textNode :: String -> [X.Node]
textNode txt = [X.TextNode . T.pack $ txt]
-}
