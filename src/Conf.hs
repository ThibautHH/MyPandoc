{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Conf
-}

{-# LANGUAGE ViewPatterns #-}

module Conf (Conf(Conf, inputFile, outputFile,
        inputFormat, outputFormat), confParser) where

import Data.List (stripPrefix)
import Options.Applicative

data DocumentFormat = XML | JSON | Markdown
    deriving (Show)

instance Read DocumentFormat where
    readsPrec _ (stripPrefix "xml" -> Just rest) = [(XML, rest)]
    readsPrec _ (stripPrefix "json" -> Just rest) = [(JSON, rest)]
    readsPrec _ (stripPrefix "markdown" -> Just rest) = [(Markdown, rest)]
    readsPrec _ _ = []

data Conf = Conf {
    inputFile :: String,
    outputFile :: Maybe String,
    inputFormat :: Maybe DocumentFormat,
    outputFormat :: DocumentFormat
} deriving (Show)

inputFileOption :: Mod OptionFields a
inputFileOption = long "input"
                    <> short 'i'
                    <> help "Input document"
                    <> metavar "INPUT"

outputFileOption :: Mod OptionFields (Maybe a)
outputFileOption = long "output"
                    <> short 'o'
                    <> help "Output document"
                    <> metavar "OUTPUT"
                    <> value Nothing

inputFormatOption :: Mod OptionFields (Maybe a)
inputFormatOption = long "input-format"
                    <> short 'e'
                    <> help "Input format"
                    <> metavar "INPUTFORMAT"
                    <> value Nothing

outputFormatOption :: Mod OptionFields a
outputFormatOption = long "output-format"
                    <> short 'f'
                    <> help "Output format"
                    <> metavar "OUTPUTFORMAT"

confParser :: Parser Conf
confParser = Conf
            <$> strOption inputFileOption
            <*> option auto outputFileOption
            <*> option auto inputFormatOption
            <*> option auto outputFormatOption
