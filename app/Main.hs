{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Main
-}

module Main (main) where

import Data.Maybe (fromMaybe)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, openFile, IOMode(WriteMode))
import Options.Applicative

import Conf (confParser, Conf(..))
import ConfUtils (getFormat)
import Encode (encode)
import Decode (decode)


main :: IO ()
main = do
    conf <- execParser (info (confParser <**> helper) $ failureCode 84)
    content <- readFile $ inputFile conf
    handle <- openFile (fromMaybe "/dev/stdout" $ outputFile conf) WriteMode
    case decode (getFormat conf content) content of
        Nothing -> exitWith $ ExitFailure 84
        Just document -> hPutStrLn handle $ encode conf document
