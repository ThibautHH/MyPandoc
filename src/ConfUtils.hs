{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ConfUtils
-}

module ConfUtils (getFormat) where

import Conf (Conf(..), DocumentFormat(..))

getFormat :: Conf -> String -> Conf
getFormat conf@(Conf{inputFormat=Just _}) _ = conf
getFormat conf ('<':_) = conf {inputFormat = Just XML}
getFormat conf ('{':_) = conf {inputFormat = Just JSON}
getFormat conf ('-':_) = conf {inputFormat = Just Markdown}
getFormat conf _ = conf
