{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Decode
-}

module Decode (decode) where

import Conf (Conf(..))

decode :: Conf -> String -> String
decode Conf {inputFormat=Nothing} _ = ""
decode conf content = content
