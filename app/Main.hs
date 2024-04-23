{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Main
-}

module Main (main) where

import Options.Applicative

import Conf (confParser, Conf(..))
import ConfUtils (getFormat)
import Encode (encode)
import Decode (decode)


main :: IO ()
main = do
    conf <- execParser (info (confParser <**> helper) $ failureCode 84)
    content <- readFile (inputFile conf)
    let decodedContent = decode (getFormat conf content) content
    putStrLn $ encode conf decodedContent
