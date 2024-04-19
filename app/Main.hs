{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Main
-}

module Main (main) where

import Options.Applicative

import Conf (confParser)

main :: IO ()
main = execParser (info (confParser <**> helper) $ failureCode 84) >>= print
