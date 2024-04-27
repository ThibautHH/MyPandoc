{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Main
-}

module Main (main) where

import System.Exit (exitWith, ExitCode(ExitFailure))
import Options.Applicative

import Conf (confParser, Conf(..))
import ConfUtils (getFormat)
import Encode (encode)
import Decode (decode)
import Parsing

printResult :: Maybe (String, String) -> IO()
printResult (Just (x, y)) = putStrLn $ "(Just (" ++ show x ++ ", " ++ show y ++ "))"
printResult Nothing = putStrLn "Nothing"

main :: IO ()
main = do
    conf <- execParser (info (confParser <**> helper) $ failureCode 84)
    content <- readFile $ inputFile conf
    let result = parseSome (parseAnyChar ['<', 'd', 'o', 'c', 'u', 'm', 'e', 'n', 't','>']) content
    printResult result
    case decode (getFormat conf content) content of
        Nothing -> exitWith $ ExitFailure 84
        Just document -> print document >> (putStrLn $ encode conf document)
