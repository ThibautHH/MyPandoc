{--
-- EPITECH PROJECT, 2024
-- FUN-PANDOC
-- File description:
-- Parsing
--}

module Parsing where

import Data.Char

type Parser a = String -> Maybe (a, String)

parseChar :: Char -> Parser Char
parseChar _ "" = Nothing
parseChar c (x:xs)
            | c == x = Just (x, xs)
            | otherwise = Nothing

parseAnyChar :: String  -> Parser Char
parseAnyChar _ "" = Nothing
parseAnyChar str (x:xs)
              | x `elem` str = Just (x, xs)
              | otherwise = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 input =
    case p1 input of
        Just result -> Just result
        Nothing -> p2 input

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 input = 
    case p1 input of
        Just (result1, rest) ->
            case p2 rest of
                Just (result2, rest2) -> Just ((result1, result2), rest2)
                Nothing -> Nothing
        Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c 
parseAndWith fct p1 p2 input =
    case p1 input of 
        Just (result1, rest) ->
            case p2 rest of
                Just (result2, rest2) -> Just ((fct result1 result2), rest2)
                Nothing -> Nothing
        Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p input =
    case p input of
        Just (result1, rest1) ->
            case parseMany p rest1 of
                Just (result2, rest2) -> Just (result1 : result2, rest2)
                Nothing -> Nothing
        Nothing -> Just ([], input)

parseSome :: Parser a -> Parser [a]
parseSome p input = 
    case parseMany p input of
        Just ([], _) -> Nothing
        result -> result

parseUInt :: Parser Int
parseUInt input =
    case input of
        (x:xs) | isDigit x ->
            let (digits, rest) = span isDigit input
            in Just (read digits, rest)
        _ -> Nothing

parseInt :: Parser Int 
parseInt input =
    case input of
        (x:xs) | isDigit x ->
            let (digits, rest) = span isDigit input
            in Just (read digits, rest)
        _ -> Nothing
