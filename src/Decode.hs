{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Decode
-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Decode (decode) where

import Control.Arrow(Arrow(first))
import Data.List(stripPrefix)

import Conf(Conf(..), DocumentFormat (..))
import Lib(Document(Document), Header(Header), Body(Body),
    Paragraph (Paragraph),
    Element(LinkElement, ImageElement),
        Link(Link),
        Image(Image))

data State = State {
    bold :: Bool,
    italic :: Bool,
    code :: Bool,
    inLink :: Bool,
    inImage :: Bool
} deriving (Show)

data Parser a = Parser {
    parse :: a,
    state :: State,
    rest :: String
} deriving (Show, Functor)

splitOn :: Eq a => [a] -> [a] -> ([a], [a])
splitOn needle (stripPrefix needle -> Just r) = ([], r)
splitOn _ [] = ([], [])
splitOn needle (x:xs) = first (x:) $ splitOn needle xs

parseLink :: Conf -> Parser () -> Maybe (Parser Link)
parseLink _ (Parser _ (State _ _ _ True _) _) = Nothing
parseLink conf@(Conf{inputFormat=Just Markdown}) (elementsParser conf "[" "](" -> Just p) =
    Just (Parser (Link (parse p) url) (state p) r)
    where
        (url, r) = splitOn ")" $ rest p
parseLink _ _ = Nothing

parseImage :: Conf -> Parser () -> Maybe (Parser Image)
parseImage _ (Parser _ (State _ _ _ True _) _) = Nothing
parseImage _ (Parser _ (State _ _ _ _ True) _) = Nothing
parseImage conf@(Conf{inputFormat=Just Markdown}) (elementsParser conf "![" "](" -> Just p) =
    Just (Parser (Image (parse p) src) (state p) r)
    where
        (src, r) = splitOn ")" $ rest p
parseImage _ _ = Nothing

parseElement :: Conf -> Parser () -> Maybe (Parser Element)
parseElement conf (parseImage conf -> Just parser) = Just $ fmap ImageElement parser
parseElement conf (parseLink conf -> Just parser) = Just $ fmap LinkElement parser
parseElement _ _ = Nothing

elementsParser :: Conf -> String -> String -> Parser () -> Maybe (Parser [Element])
elementsParser conf start stop (Parser _ s (stripPrefix start -> Just content)) =
    parseElements conf stop (Parser [] s content)
elementsParser _ _ _ _ = Nothing

parseElements :: Conf -> String -> Parser [Element] -> Maybe (Parser [Element])
parseElements _ _ p@(Parser _ _ "") = Just p
parseElements _ stop (Parser ls s (stripPrefix stop -> Just r)) = Just (Parser ls s r)
parseElements conf stop (Parser ls s content) =
    (\(Parser element s' r) -> parseElements conf stop (Parser (element:ls) s' r)) =<< parseElement conf (Parser () s content)

paragraphStart :: DocumentFormat -> String
paragraphStart Markdown = ""
paragraphStart XML = "<paragraph>"
paragraphStart JSON = "["

paragraphStop :: DocumentFormat -> String
paragraphStop Markdown = "\n\n"
paragraphStop XML = "</paragraph>"
paragraphStop JSON = "]"

parseParagraph :: Conf -> Parser () -> Maybe (Parser Paragraph)
parseParagraph conf@(Conf{inputFormat=Just format})
    (elementsParser conf (paragraphStart format) (paragraphStop format) -> Just p) = Just $ fmap Paragraph p
parseParagraph _ _ = Nothing

decode :: Conf -> String -> Maybe Document
decode Conf{inputFormat=Nothing} _ = Nothing
decode _ _ = Just $ Document (Header "Document" Nothing Nothing) (Body [])
    -- . Body . parse <$> parseContainers conf emptyParser
    -- where
    --     emptyParser = Parser () (State False False False) content
