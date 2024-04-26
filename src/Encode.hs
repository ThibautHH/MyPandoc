{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Encode
-}

{-# LANGUAGE NamedFieldPuns #-}

module Encode (encode) where

import Data.Function ((&))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import Conf(Conf(..), DocumentFormat(..))
import Lib(Document(..), Header(..), Body(..),
    Container(..),
        Section(name, content),
        List(items),
        CodeBlock(language, blocks),
    Paragraph(..),
    Element(..),
        Text(text, bold, italic, code),
        Link(display, url),
        Image(alt, src))

insertAuthor :: Maybe String -> String
insertAuthor Nothing = ""
insertAuthor (Just author) = "\n    <author>" ++ author ++ "</author>"

insertDate :: Maybe String -> String
insertDate Nothing = ""
insertDate (Just date) = "\n    <date>" ++ date ++ "</date>"

appendMaybe :: Maybe [a] -> [a] -> [a]
appendMaybe x a = a ++ fromMaybe [] x


addHeader :: Conf -> Header -> String
addHeader Conf{outputFormat=Markdown} Header{title, author, date} =
    intercalate "\n" [
        "---",
        "title: " ++ title &
        appendMaybe (fmap ("\nauthor: " ++) author) &
        appendMaybe (fmap ("\ndate: " ++) date),
        "---"
    ]
addHeader Conf{outputFormat=JSON} Header{title, author, date} =
    intercalate "" [
        "\"header\": {",
        "\n    \"title\": \"" ++ title &
        appendMaybe (fmap ("\",\n    \"author\": \"" ++) author) &
        appendMaybe (fmap ("\",\n    \"date\": \"" ++) date),
        "\"\n},"
    ]
addHeader Conf{outputFormat=XML} Header{title, author, date} =
    "<header title:\"" ++ title ++ "\">" ++
    insertAuthor author ++ insertDate date ++ "\n</header>"


addParagraph :: Conf -> Paragraph -> String
addParagraph Conf{outputFormat=Markdown} Paragraph{elements} = ""
addParagraph Conf{outputFormat=JSON} Paragraph{elements} = ""
addParagraph Conf{outputFormat=XML} Paragraph{elements} = ""


addContainer :: Conf -> Container -> String
addContainer Conf{outputFormat=Markdown} _ = ""
addContainer Conf{outputFormat=JSON} _ = ""
addContainer Conf{outputFormat=XML} _ = ""


diffPC :: Conf -> Either Container Paragraph -> String
diffPC conf (Left container) = addContainer conf container
diffPC conf (Right paragraph) = addParagraph conf paragraph


addBody :: Conf -> Body -> String
addBody Conf{outputFormat=Markdown} Body{sections} = ""
addBody Conf{outputFormat=JSON} Body{sections} = ""
addBody Conf{outputFormat=XML} Body{sections} = ""

encode :: Conf -> Document -> String
encode conf doc = addHeader conf (header doc) ++ addBody conf (body doc)
