{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Decode
-}

{-# LANGUAGE ViewPatterns #-}

module Decode (decode) where

import Control.Arrow(Arrow(first))
import Control.Applicative((<|>), Alternative(empty), optional)
import Control.Monad((>=>))
import Data.List(stripPrefix)

import Conf(Conf(..), DocumentFormat (..))
import Lib(Document(Document), Header(Header), Body(Body),
    Container(SectionContainer, ListContainer, CodeBlockContainer),
        Section(Section),
        List(List),
        CodeBlock(CodeBlock),
    Paragraph (Paragraph),
    Element(LinkElement, ImageElement),
        Link(Link),
        Image(Image))

data State = State {
    rest :: String,
    bold :: Bool,
    italic :: Bool,
    code :: Bool,
    inLink :: Bool,
    inImage :: Bool,
    sectionLevel :: Int
} deriving (Show)

defaultState :: String -> State
defaultState x = State x False False False False False 0

type Result a = Maybe (a, State)

newtype Parser a = Parser {
    parse :: State -> Result a
}

instance Functor Parser where
    fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
    pure x = Parser (\s -> Just (x, s))
    Parser fp <*> Parser xp = Parser (fp >=> (\(f, s) -> xp s >>= (\(x, s') -> Just (f x, s'))))

instance Alternative Parser where
    empty = Parser (const Nothing)
    Parser p1 <|> Parser p2 = Parser (\s -> p1 s <|> p2 s)

instance Monad Parser where
    Parser p >>= f = Parser (p >=> (\(x, s) -> parse (f x) s))

splitOn :: Eq a => [a] -> [a] -> ([a], [a])
splitOn needle (stripPrefix needle -> Just r) = ([], r)
splitOn _ [] = ([], [])
splitOn needle (x:xs) = first (x:) $ splitOn needle xs

confWhitespace :: Conf -> String
confWhitespace Conf{inputFormat=Just Markdown} = "\n\v\f"
confWhitespace _ = " \t\n\f\v"

parseWhitespace :: Conf -> Parser String
parseWhitespace (confWhitespace -> whtspc) =
    Parser (\s@(State{rest=(span (`elem` whtspc) -> (x, r))}) -> Just (x, s{rest=r}))

parseString :: String -> Parser String
parseString x = Parser (\s@(State{rest=(stripPrefix x -> res)}) -> (\r -> (x, s{rest=r})) <$> res)

parseUntil :: String -> Parser String
parseUntil stop = Parser (\s@(State{rest=(splitOn stop -> (x, r))}) -> Just (x, s{rest=r}))

parseElement :: Conf -> Parser () -> Maybe (Parser Element)
parseElement conf (parseImage conf -> Just parser) = Just $ fmap ImageElement parser
parseElement conf (parseLink conf -> Just parser) = Just $ fmap LinkElement parser
parseElement _ _ = Nothing

parseBody :: Conf -> Parser Body
parseBody _ = Parser $ const (Just (Body [], defaultState ""))

parseJsonString :: Parser String
parseJsonString = parseString "\"" *> parseUntil "\"" <* parseString "\""

parseJsonField :: Conf -> String -> Parser String
parseJsonField conf field = parseString "\"" *> parseString field *> parseString "\"" *> parseWhitespace conf *> parseString ":" <* parseWhitespace conf

parseJsonSeparator :: Conf -> Parser (Maybe String)
parseJsonSeparator conf = parseWhitespace conf *> optional (parseString ",") <* parseWhitespace conf

parseStart :: Conf -> Parser String
parseStart conf@(Conf{inputFormat=Just Markdown}) = parseString "---\n" <* parseWhitespace conf
parseStart conf@(Conf{inputFormat=Just XML}) = parseString "<document>" *> parseWhitespace conf *> parseString "<header>" <* parseWhitespace conf
parseStart conf@(Conf{inputFormat=Just JSON}) = parseString "{" *> parseWhitespace conf *> parseJsonField conf "header" <* parseString "{" <* parseWhitespace conf
parseStart _ = Parser $ const Nothing

parserHeaderField :: Conf -> String -> Parser String
parserHeaderField conf@(Conf{inputFormat=Just Markdown}) field = parseString field *> parseString ": " *> parseUntil "\n" <* parseWhitespace conf
parserHeaderField conf@(Conf{inputFormat=Just XML}) field = parseString (('<':field) ++ ">") *> parseUntil ("</" ++ field ++ ">") <* parseWhitespace conf
parserHeaderField conf@(Conf{inputFormat=Just JSON}) field = parseJsonField conf field *> parseJsonString <* parseJsonSeparator conf
parserHeaderField _ _ = Parser $ const Nothing

parseHeaderEnd :: Conf -> Parser String
parseHeaderEnd conf@(Conf{inputFormat=Just Markdown}) = parseString "---\n" <* parseWhitespace conf
parseHeaderEnd conf@(Conf{inputFormat=Just XML}) = parseString "</header>" <* parseWhitespace conf
parseHeaderEnd conf@(Conf{inputFormat=Just JSON}) = parseString "}" <* parseWhitespace conf
parseHeaderEnd _ = Parser $ const Nothing

parseTitle :: Conf -> Parser String
parseTitle conf = parserHeaderField conf "title"

parseAuthor :: Conf -> Parser String
parseAuthor conf = parserHeaderField conf "author"

parseDate :: Conf -> Parser String
parseDate conf = parserHeaderField conf "date"

parseHeader :: Conf -> Parser Header
parseHeader conf = Header <$>
    (parseStart conf *> parseTitle conf)
        <*> optional (parseAuthor conf)
        <*> optional (parseDate conf)
        <* parseHeaderEnd conf

decode :: Conf -> String -> Maybe Document
decode Conf{inputFormat=Nothing} _ = Nothing
decode conf content = fst <$> parse (Document <$> parseHeader conf <*> parseBody conf) (defaultState content)
