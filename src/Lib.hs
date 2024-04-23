{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Lib
-}

module Lib (Document(..), Header(..), Body(..),
    Container(..),
        Section(name, content),
        List(items),
        CodeBlock(language, blocks),
    Paragraph(..),
    Element(..),
        Text(text, bold, italic, code),
        Link(display, url),
        Image(alt, src)) where

data Document = Document {
    header :: Header,
    body :: Body
} deriving (Show)

data Header = Header {
    title :: String,
    author :: Maybe String,
    date :: Maybe String
} deriving (Show)

newtype Body = Body {
    sections :: [Either Container Paragraph]
} deriving (Show)

data Container = SectionContainer Section | ListContainer List | CodeBlockContainer CodeBlock
    deriving (Show)

data Section = Section {
    name :: String,
    content :: [Either Container Paragraph]
} deriving (Show)

newtype List = List {
    items :: [Paragraph]
} deriving (Show)

data CodeBlock = CodeBlock {
    language :: Maybe String,
    blocks :: [Paragraph]
} deriving (Show)

newtype Paragraph = Paragraph {
    elements :: [Element]
} deriving (Show)

data Element = TextElement Text | LinkElement Link | ImageElement Image
    deriving (Show)

data Text = Text {
    text :: String,
    bold :: Bool,
    italic :: Bool,
    code :: Bool
} deriving (Show)

data Link = Link {
    display :: [Element],
    url :: String
} deriving (Show)

data Image = Image {
    alt :: [Element],
    src :: String
} deriving (Show)
