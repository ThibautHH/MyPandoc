{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Main
-}

module Main (main) where

import Options.Applicative

import Conf (confParser, Conf(..), DocumentFormat(..))

getString :: Char -> Char -> String -> String
getString _ _ [] = ""
getString fc lc (x:xs) | x == lc = ""
                       | otherwise = x:getString fc lc xs

getTokens :: Char -> Char -> String -> String
getTokens _ _ [] = ""
getTokens fc lc (x:xs) | x == fc = getString fc lc xs
                       | otherwise = getTokens fc lc xs

encode :: Conf -> String -> String
encode conf content = content

decode :: Conf -> String -> String
decode Conf {inputFormat=Nothing} _ = ""
decode conf content = content

getFomat :: Conf -> String -> Conf
getFomat conf@(Conf{inputFormat=Just _}) _ = conf
getFomat conf ('<':_) = conf {inputFormat = Just XML}
getFomat conf ('{':_) = conf {inputFormat = Just JSON}
getFomat conf ('-':_) = conf {inputFormat = Just Markdown}
getFomat conf _ = conf

main :: IO ()
main = do
    conf <- execParser (info (confParser <**> helper) $ failureCode 84)
    content <- readFile (inputFile conf)
    let decodedStr = decode (getFomat conf content) $ getTokens '{' '}' content
    putStrLn $ encode conf decodedStr
