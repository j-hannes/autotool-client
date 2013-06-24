{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- |
module Modules.Student.View.Solution
    ( bindFormSplices
    , solutionFormTemplate
    ) where

------------------------------------------------------------------------------
import           Data.ByteString                (ByteString)
-- import qualified Data.ByteString.Char8          as BS
import           Data.List                      (intersperse)
import qualified Data.Text                      as T
import           Heist
import qualified Heist.Interpreted              as I
import           Heist.Interpreted              (Splice)
import qualified Text.XmlHtml                   as X
------------------------------------------------------------------------------
import           Application
import           Model.Datatypes


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
bindFormSplices :: StudentId
                -> String
                -> [(String, String)]
                -> Maybe String
                -> Maybe String
                -> SpliceBinder 
bindFormSplices sid taskDescription doc eva err =
    I.bindSplices
      [ ("taskDescription",   I.textSplice $ T.pack taskDescription)
      , ("taskDocumentation", taskDocumentationSplice doc)
      , ("verificationError", verificationErrorSplice err)
      , ("evaluation",        evaluationSplice eva)
      , ("studentId",         I.textSplice $ T.pack sid)
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
