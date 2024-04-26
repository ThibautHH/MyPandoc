{--
-- EPITECH PROJECT, 2024
-- FUN-PANDOC
-- File description:
-- Spec
--}

import Test.Hspec

import Parsing

main :: IO()
main = hspec $ do
    describe "parserAtomic" $ do
        it "takes a Char as argument and returns a Parser Char" $ do
            parseChar 'a' "abcd" `shouldBe` (Just ('a', "bcd"))
            parseChar 'z' "abcd" `shouldBe` (Nothing)
            parseChar 'b' "abcd" `shouldBe` (Nothing)
            parseChar 'a' "aaaa" `shouldBe` (Just ('a', "aaa"))

        it "parse any of the characters in the string in its first argument" $ do
            parseAnyChar "bca" "abcd" `shouldBe` (Just ('a', "bcd"))
            parseAnyChar "xyz" "abcd" `shouldBe` (Nothing)
            parseAnyChar "bca" "cdef" `shouldBe` (Just ('c', "def"))
       
        it "takes two parsers in argument, tries to apply the first one, and if it fails, try to apply the second one" $ do 
            parseOr (parseChar 'a') (parseChar 'b') "abcd" `shouldBe` (Just ('a', "bcd"))
            parseOr (parseChar 'a') (parseChar 'b') "bcda" `shouldBe` (Just ('b', "cda"))
            parseOr (parseChar 'a') (parseChar 'b') "xyz" `shouldBe` (Nothing)

        it "takes two parsers, tries to apply the first one, and if it succeed, applies the second one and returns a tuple of what it parsed" $ do
            parseAnd (parseChar 'a') (parseChar 'b') "abcd" `shouldBe` (Just (('a', 'b'), "cd"))
            parseAnd (parseChar 'a') (parseChar 'b') "bcda" `shouldBe` (Nothing)
            parseAnd (parseChar 'a') (parseChar 'b') "acd" `shouldBe` (Nothing)

        it "takes an additional function which gets the parsed elements as arguments" $ do
            parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b') "abcd" `shouldBe` (Just ("ab", "cd"))

        it "takes a parser in argument and tries to apply it zero or more times, returning a list of the parsed elements" $ do
            parseMany (parseChar ' ') "    foobar" `shouldBe` (Just ("    ", "foobar"))
            parseMany (parseChar ' ') "foobar    " `shouldBe` (Just("", "foobar    "))

        it "like parseMany but must parse at least one element and fails otherwise" $ do
            parseSome (parseAnyChar ['0'..'9']) "42foobar" `shouldBe` (Just ("42", "foobar"))
            parseSome (parseAnyChar ['0'..'9']) "foobar42" `shouldBe` (Nothing)

        it "UInt" $ do
            parseUInt "123foo bar" `shouldBe` (Just (123, "foo bar"))