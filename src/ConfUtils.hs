{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ConfUtils
-}

module ConfUtils (getFomat) where

import Conf (Conf(..), DocumentFormat(..))

getFomat :: Conf -> String -> Conf
getFomat conf@(Conf{inputFormat=Just _}) _ = conf
getFomat conf ('<':_) = conf {inputFormat = Just XML}
getFomat conf ('{':_) = conf {inputFormat = Just JSON}
getFomat conf ('-':_) = conf {inputFormat = Just Markdown}
getFomat conf _ = conf
