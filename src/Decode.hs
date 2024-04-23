{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Decode
-}

module Decode (decode) where

import Conf(Conf(..))
import Lib(Document(Document), Header(Header), Body(Body))

decode :: Conf -> String -> Maybe Document
decode Conf{inputFormat=Nothing} _ = Nothing
decode _ _ = Just $ Document (Header "Document" Nothing Nothing) (Body [])
