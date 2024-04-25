{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Main
-}

module Main (main) where

import System.Exit (exitWith, ExitCode(ExitFailure))
import Options.Applicative

import Lib (getDocument)
import Conf (confParser, Conf(..))
import ConfUtils (getFormat)
import Encode (encode)
import Decode (decode)


main :: IO ()
main = do
    conf <- execParser (info (confParser <**> helper) $ failureCode 84)
    content <- readFile $ inputFile conf
    let doc = getDocument conf content
    print doc
