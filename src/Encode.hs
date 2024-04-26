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


indent :: Int -> String
indent i = replicate (i * 4) ' '

addHeader :: DocumentFormat -> Header -> Int -> String
addHeader Markdown Header{title, author, date} iLvl =
    intercalate "\n" [
        "---",
        "title: " ++ title &
        appendMaybe (fmap ("\nauthor: " ++) author) &
        appendMaybe (fmap ("\ndate: " ++) date),
        "---"
    ]
addHeader JSON Header{title, author, date} iLvl =
    intercalate "" [
        indent iLvl ++ "\"header\": {",
        "\n" ++ indent (iLvl+1) ++ "\"title\": \"" ++ title &
        appendMaybe (fmap (("\",\n" ++ indent (iLvl+1) ++ "\"author\": \"") ++) author) &
        appendMaybe (fmap (("\",\n" ++ indent (iLvl+1) ++ "\"date\": \"") ++) date),
        "\"\n" ++ indent iLvl ++ "},"
    ]
addHeader XML Header{title, author, date} _ =
    "<header title:\"" ++ title ++ "\">" ++
    insertAuthor author ++ insertDate date ++ "\n</header>"


addElement :: DocumentFormat -> Element -> Int -> String
addElement Markdown (TextElement elem) _ = 
    let
        b = if bold elem then "**" else ""
        i = if italic elem then "*" else ""
        c = if code elem then "`" else ""
    in b ++ i ++ c ++ (text elem) ++ c ++ i ++ b
addElement Markdown (LinkElement link) _ =
    "[" ++ (foldMap (addElement Markdown) (display link) 0) ++ "](" ++ (url link) ++ ")"
addElement Markdown (ImageElement image) _ =
    "![" ++ (foldMap (addElement Markdown) (alt image) 0) ++ "](" ++ (src image) ++ ")"
addElement JSON (TextElement elem) iLvl | (bold elem) =
    ",\n" ++ indent iLvl ++ "{\n" ++ indent (iLvl+1) ++ "\"bold\": \"" ++ (text elem) ++ "\"\n" ++ indent (iLvl) ++ "},\n"
                                        | (italic elem) =
    ",\n" ++ indent iLvl ++ "{\n" ++ indent (iLvl+1) ++ "\"italic\": \"" ++ (text elem) ++ "\"\n" ++ indent (iLvl) ++ "},\n"
                                        | (code elem) =
    ",\n" ++ indent iLvl ++ "{\n" ++ indent (iLvl+1) ++ "\"code\": \"" ++ (text elem) ++ "\"\n" ++ indent (iLvl) ++ "},\n"
                                        | otherwise =
    indent (iLvl) ++ "\"" ++ (text elem) ++ "\""
addElement JSON (LinkElement link) iLvl =
    ",\n" ++ indent iLvl ++ "{\n" ++ indent (iLvl+1) ++ "\"link\": {\n" ++ indent (iLvl+2) ++ "\"url\": \"" ++ (url link) ++ "\",\n" ++ indent (iLvl+2) ++ "\"content\": [\n" ++ indent (iLvl+3) ++ (foldMap (addElement JSON) (display link) 0) ++ "\n" ++ indent (iLvl+2) ++ "]\n" ++ indent (iLvl+1) ++ "}\n" ++ indent (iLvl) ++ "},\n"
addElement JSON (ImageElement image) iLvl =
    ",\n" ++ indent iLvl ++ "{\n" ++ indent (iLvl+1) ++ "\"image\": {\n" ++ indent (iLvl+2) ++ "\"url\": \"" ++ (src image) ++ "\",\n" ++ indent (iLvl+2) ++ "\"alt\": [\n" ++ indent (iLvl+3) ++ (foldMap (addElement JSON) (alt image) 0) ++ "\n" ++ indent (iLvl+2) ++ "]\n" ++ indent (iLvl+1) ++ "}\n" ++ indent (iLvl) ++ "},\n"
addElement XML (TextElement elem) iLvl = 
    let
        b = if bold elem then "**" else ""
        i = if italic elem then "*" else ""
        c = if code elem then "`" else ""
    in b ++ i ++ c ++ (text elem) ++ c ++ i ++ b
addElement XML (LinkElement link) iLvl = ""
addElement XML (ImageElement image) iLvl = ""


addParagraph :: DocumentFormat -> Paragraph -> Int -> String
addParagraph Markdown Paragraph{elements} _ =
    "\n" ++ foldMap (addElement Markdown) elements 0
addParagraph JSON Paragraph{elements} iLvl =
    indent (iLvl+1) ++ "[\n" ++ foldMap (addElement JSON) elements (iLvl+2) ++ "\n" ++ indent (iLvl+1) ++ "],"
addParagraph XML Paragraph{elements} _ = ""

addListElement :: DocumentFormat -> Paragraph -> Int -> String
addListElement Markdown Paragraph{elements} _ =
    "\n- " ++ foldMap (addElement Markdown) elements 0
addListElement JSON Paragraph{elements} iLvl =
    "\n" ++ indent (iLvl+1) ++ "[\n" ++ foldMap (addElement JSON) elements (iLvl+2) ++ "\n" ++ indent (iLvl+1) ++ "],"
addListElement XML Paragraph{elements} _ = ""


addList :: DocumentFormat -> Container -> Int -> String
addList Markdown (ListContainer elem) _ =
    foldMap (addListElement Markdown) (items elem) 0
addList JSON (ListContainer elem) i =
    "\n" ++ indent (i+1) ++ "{\n" ++ indent (i+2) ++ "\"list\": [" ++ init (foldMap (addListElement JSON) (items elem) (i+2)) ++ "\n" ++ indent (i+2) ++ "]\n" ++ indent (i+1) ++ "},"
addList XML (ListContainer elem) _ = ""


addSection :: DocumentFormat -> Container -> Int -> String
addSection Markdown (SectionContainer elem) i | (name elem) /= "" =
    "\n\n" ++ replicate i '#' ++ " " ++ (name elem) ++ foldMap (diffPC Markdown) (content elem) (i+1)
                                              | otherwise = foldMap (diffPC Markdown) (content elem) (i+1)
addSection JSON (SectionContainer elem) i =
    "\n" ++
    indent (i+1) ++ "{\n" ++
    indent (i+2) ++ "\"section\": {\n" ++
    indent (i+3) ++ "\"title\": \"" ++ (name elem) ++ "\",\n" ++
    indent (i+3) ++ "\"content\": [" ++ init (foldMap (diffPC JSON) (content elem) (i+3)) ++ "\n" ++
    indent (i+3) ++ "]\n" ++
    indent (i+2) ++ "}\n" ++
    indent (i+1) ++ "}\n"
addSection XML (SectionContainer elem) i = ""


addCodeBlock :: DocumentFormat -> Container -> Int -> String
addCodeBlock Markdown (CodeBlockContainer elem) iLvl =
    "\n\n```" ++ foldMap (addParagraph Markdown) (blocks elem) iLvl ++ "\n```"
addCodeBlock JSON (CodeBlockContainer elem) iLvl =
    "\n" ++ indent (iLvl+1) ++ "{\n" ++ indent (iLvl+2) ++ "\"codeblock\": [\n" ++ init (foldMap (addParagraph JSON) (blocks elem) (iLvl+2)) ++ "\n" ++ indent (iLvl+2) ++ "]" ++ "\n" ++ indent (iLvl+1) ++ "},"
addCodeBlock XML (CodeBlockContainer elem) iLvl = ""


addContainer :: DocumentFormat -> Container -> Int -> String
addContainer form cont@(SectionContainer _) iLvl = addSection form cont iLvl
addContainer form cont@(ListContainer _) iLvl = addList form cont iLvl
addContainer form cont@(CodeBlockContainer _) iLvl = addCodeBlock form cont iLvl


diffPC :: DocumentFormat -> Either Container Paragraph -> Int -> String
diffPC form (Left container) iLvl = addContainer form container iLvl
diffPC form (Right paragraph) iLvl = "\n" ++ addParagraph form paragraph iLvl


addBody :: DocumentFormat -> Body -> String
addBody _ Body{sections=[]} = ""
addBody Markdown Body{sections} = foldMap (diffPC Markdown) sections 1
addBody JSON Body{sections} = "\n    \"body\": [" ++ init (foldMap (diffPC JSON) sections 1) ++ "\n    ]"
addBody XML Body{sections} = foldMap (diffPC XML) sections 1

encode :: Conf -> Document -> String
encode Conf{outputFormat=Markdown} doc = 
    addHeader Markdown (header doc) 1 ++ addBody Markdown (body doc)
encode Conf{outputFormat=JSON} doc =
    "{\n" ++ addHeader JSON (header doc) 1 ++ addBody JSON (body doc) ++ "\n}"
encode Conf{outputFormat=XML} doc =
    "<Document>\n" ++ addHeader XML (header doc) 1 ++ addBody XML (body doc) ++ "\n</Document>"
