{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Decode
-}

module Decode (decode) where

import Conf(Conf(..))
import Lib(Document(Document), Header(Header), Body(Body))

decode :: Conf -> String -> Maybe Document
decode Conf{inputFormat=Nothing} _ = Nothing
decode _ content = Just $ Document (Header (getXMLTitle content) (getXMLAuthors content) (getXMLDate content)) (Body [])

getXMLTitle :: String -> String
getXMLTitle content =
    let startTag = "<header title=\""
        endTag = "\">"
        authorsStart = drop (length startTag) $ snd $ break (== startTag) $ words content
        authorsEnd = takeWhile (/= endTag) authorsStart
    in unwords authorsEnd

getXMLAuthors :: String -> Maybe String
getXMLAuthors content =
    let startTag = "<author>"
        endTag = "<\\author>"
        authorsStart = drop (length startTag) $ snd $ break (== startTag) $ words content
        authorsEnd = takeWhile (/= endTag) authorsStart
    in if null authorsEnd then Nothing else Just (unwords authorsEnd)

getXMLDate :: String -> Maybe String
getXMLDate content =
    let startTag = "<date>"
        endTag = "<\\date>"
        authorsStart = drop (length startTag) $ snd $ break (== startTag) $ words content
        authorsEnd = takeWhile (/= endTag) authorsStart
    in if null authorsEnd then Nothing else Just (unwords authorsEnd)