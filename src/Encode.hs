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
        CodeBlock(..),
    Paragraph(..),
    Element(..),
        Text(text, bold, italic, code),
        Link(display, url),
        Image(alt, src))

insertAuthor :: Maybe String -> String
insertAuthor Nothing = ""
insertAuthor (Just author) =
    "\n" ++ indent 2 ++ "<author>" ++ author ++ "</author>"


insertDate :: Maybe String -> String
insertDate Nothing = ""
insertDate (Just date) =
    "\n" ++ indent 2 ++ "<date>" ++ date ++ "</date>"

appendMaybe :: Maybe [a] -> [a] -> [a]
appendMaybe x a = a ++ fromMaybe [] x


indent :: Int -> String
indent i = replicate (i * 4) ' '

addHeader :: DocumentFormat -> Header -> Int -> String
addHeader Markdown Header{title, author, date} _ =
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
    "    <header title=\"" ++ title ++ "\">" ++
    insertAuthor author ++ insertDate date ++ "\n    </header>"


addElement :: DocumentFormat -> Element -> Int -> String
addElement Markdown (TextElement elemE) _ = 
    let
        b = if bold elemE then "**" else ""
        i = if italic elemE then "*" else ""
        c = if code elemE then "`" else ""
    in b ++ i ++ c ++ (text elemE) ++ c ++ i ++ b
addElement Markdown (LinkElement link) _ =
    "[" ++ (foldMap (addElement Markdown) (display link) 0) ++ "](" ++ (url link) ++ ")"
addElement Markdown (ImageElement image) _ =
    "![" ++ (foldMap (addElement Markdown) (alt image) 0) ++ "](" ++ (src image) ++ ")"
addElement JSON (TextElement elemE) iLvl | (bold elemE) =
    ",\n" ++ indent iLvl ++ "{\n" ++ indent (iLvl+1) ++ "\"bold\": \"" ++ (text elemE) ++ "\"\n" ++ indent (iLvl) ++ "},\n"
                                        | (italic elemE) =
    ",\n" ++ indent iLvl ++ "{\n" ++ indent (iLvl+1) ++ "\"italic\": \"" ++ (text elemE) ++ "\"\n" ++ indent (iLvl) ++ "},\n"
                                        | (code elemE) =
    ",\n" ++ indent iLvl ++ "{\n" ++ indent (iLvl+1) ++ "\"code\": \"" ++ (text elemE) ++ "\"\n" ++ indent (iLvl) ++ "},\n"
                                        | otherwise =
    indent (iLvl) ++ "\"" ++ (text elemE) ++ "\""
addElement JSON (LinkElement link) iLvl =
    ",\n" ++ indent iLvl ++ "{\n" ++ indent (iLvl+1) ++ "\"link\": {\n" ++ indent (iLvl+2) ++ "\"url\": \"" ++ (url link) ++ "\",\n" ++ indent (iLvl+2) ++ "\"content\": [\n" ++ indent (iLvl+3) ++ (foldMap (addElement JSON) (display link) 0) ++ "\n" ++ indent (iLvl+2) ++ "]\n" ++ indent (iLvl+1) ++ "}\n" ++ indent (iLvl) ++ "},\n"
addElement JSON (ImageElement image) iLvl =
    ",\n" ++ indent iLvl ++ "{\n" ++ indent (iLvl+1) ++ "\"image\": {\n" ++ indent (iLvl+2) ++ "\"url\": \"" ++ (src image) ++ "\",\n" ++ indent (iLvl+2) ++ "\"alt\": [\n" ++ indent (iLvl+3) ++ (foldMap (addElement JSON) (alt image) 0) ++ "\n" ++ indent (iLvl+2) ++ "]\n" ++ indent (iLvl+1) ++ "}\n" ++ indent (iLvl) ++ "},\n"
addElement XML (TextElement elemE) _ = 
    let
        b = if bold elemE then "**" else ""
        i = if italic elemE then "*" else ""
        c = if code elemE then "`" else ""
    in b ++ i ++ c ++ (text elemE) ++ c ++ i ++ b
addElement XML (LinkElement link) _ =
    "<link url=\"" ++ (url link) ++ "\">" ++ foldMap (addElement XML) (display link) 0 ++ "</link>"
addElement XML (ImageElement image) _ =
    "<image url=\"" ++ (src image) ++ "\">" ++ foldMap (addElement XML) (alt image) 0 ++ "</image>"


addParagraph :: DocumentFormat -> Paragraph -> Int -> String
addParagraph Markdown Paragraph{elements} _ =
    "\n" ++ foldMap (addElement Markdown) elements 0
addParagraph JSON Paragraph{elements} iLvl =
    indent (iLvl+1) ++ "[\n" ++ foldMap (addElement JSON) elements (iLvl+2) ++ "\n" ++ indent (iLvl+1) ++ "],"
addParagraph XML Paragraph{elements} iLvl =
    indent (iLvl+1) ++ "<paragraph>" ++ foldMap (addElement XML) elements 0 ++ "</paragraph>"

addListElement :: DocumentFormat -> Paragraph -> Int -> String
addListElement Markdown Paragraph{elements} _ =
    "\n- " ++ foldMap (addElement Markdown) elements 0
addListElement JSON Paragraph{elements} iLvl =
    "\n" ++ indent (iLvl+1) ++ "[\n" ++ foldMap (addElement JSON) elements (iLvl+2) ++ "\n" ++ indent (iLvl+1) ++ "],"
addListElement XML Paragraph{elements} i =
    "\n" ++ indent (i+1) ++ "<paragraph>" ++ foldMap (addElement XML) elements (i+2) ++ "</paragraph>"

addList :: DocumentFormat -> Container -> Int -> String
addList Markdown (ListContainer elemE) _ =
    foldMap (addListElement Markdown) (items elemE) 0
addList JSON (ListContainer elemE) i =
    "\n" ++ indent (i+1) ++ "{\n" ++ indent (i+2) ++ "\"list\": [" ++ init (foldMap (addListElement JSON) (items elemE) (i+2)) ++ "\n" ++ indent (i+2) ++ "]\n" ++ indent (i+1) ++ "},"
addList XML (ListContainer elemE) i =
    "\n" ++ indent (i+1) ++ "<list>" ++ foldMap (addListElement XML) (items elemE) (i+1) ++ "\n" ++ indent (i+1) ++ "</list>"
addList _ _ _ = ""

addSection :: DocumentFormat -> Container -> Int -> String
addSection Markdown (SectionContainer elemE) i | (name elemE) /= "" =
    "\n\n" ++ replicate i '#' ++ " " ++ (name elemE) ++
    foldMap (diffPC Markdown) (content elemE) (i+1)
                                              | otherwise =
    foldMap (diffPC Markdown) (content elemE) (i+1)
addSection JSON (SectionContainer elemE) i =
    "\n" ++
    indent (i+1) ++ "{\n" ++
    indent (i+2) ++ "\"section\": {\n" ++
    indent (i+3) ++ "\"title\": \"" ++ (name elemE) ++ "\",\n" ++
    indent (i+3) ++ "\"content\": [" ++
    init (foldMap (diffPC JSON) (content elemE) (i+3)) ++ "\n" ++
    indent (i+3) ++ "]\n" ++
    indent (i+2) ++ "}\n" ++
    indent (i+1) ++ "}\n"
addSection XML (SectionContainer elemE) i =
    "\n" ++
    indent (i+1) ++ "<section title=\"" ++ (name elemE) ++ "\">" ++
    foldMap (diffPC XML) (content elemE) (i+1) ++ "\n" ++
    indent (i+1) ++ "</section>"
addSection _ _ _ = ""


addCodeBlock :: DocumentFormat -> Container -> Int -> String
addCodeBlock Markdown (CodeBlockContainer elemE) iLvl =
    "\n\n```" ++ foldMap (addParagraph Markdown) (blocks elemE) iLvl ++ "\n```"
addCodeBlock JSON (CodeBlockContainer elemE) iLvl =
    "\n" ++
    indent (iLvl+1) ++ "{\n" ++
    indent (iLvl+2) ++ "\"codeblock\": [\n" ++
    init (foldMap (addParagraph JSON) (blocks elemE) (iLvl+2)) ++ "\n" ++
    indent (iLvl+2) ++ "]" ++ "\n" ++
    indent (iLvl+1) ++ "},"
addCodeBlock XML (CodeBlockContainer elemE) iLvl =
    "\n" ++
    indent (iLvl+1) ++ "<codeblock>\n" ++
    foldMap (addParagraph XML) (blocks elemE) (iLvl+1) ++ "\n" ++
    indent (iLvl+1) ++ "</codeblock>"
addCodeBlock _ _ _ = ""

addContainer :: DocumentFormat -> Container -> Int -> String
addContainer form cont@(SectionContainer _) i = addSection form cont i
addContainer form cont@(ListContainer _) i = addList form cont i
addContainer form cont@(CodeBlockContainer _) i = addCodeBlock form cont i


diffPC :: DocumentFormat -> Either Container Paragraph -> Int -> String
diffPC form (Left container) iLvl = addContainer form container iLvl
diffPC form (Right paragraph) iLvl = "\n" ++ addParagraph form paragraph iLvl


addBody :: DocumentFormat -> Body -> String
addBody _ Body{sections=[]} = ""
addBody Markdown Body{sections} = foldMap (diffPC Markdown) sections 1
addBody JSON Body{sections} =
    "\n" ++
    indent 1 ++ "\"body\": [" ++
    init (foldMap (diffPC JSON) sections 1) ++ "\n" ++
    indent 1 ++ "]"
addBody XML Body{sections} =
    "\n" ++
    indent 1 ++ "<body>" ++ foldMap (diffPC XML) sections 1 ++ "\n" ++
    indent 1 ++ "</body>"

encode :: Conf -> Document -> String
encode Conf{outputFormat=Markdown} doc = 
    addHeader Markdown (header doc) 1 ++ addBody Markdown (body doc)
encode Conf{outputFormat=JSON} doc =
    "{\n" ++ addHeader JSON (header doc) 1 ++ addBody JSON (body doc) ++ "\n}"
encode Conf{outputFormat=XML} doc =
    "<document>\n" ++
    addHeader XML (header doc) 1 ++ addBody XML (body doc) ++
    "\n</document>"
