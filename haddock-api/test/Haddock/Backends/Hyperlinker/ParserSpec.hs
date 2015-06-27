module Haddock.Backends.Hyperlinker.ParserSpec (main, spec) where


import Test.Hspec

import Haddock.Backends.Hyperlinker.Parser


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "parse" parseSpec


parseSpec :: Spec
parseSpec = do

    context "when parsing single-line comments" $ do

        it "should ignore content until the end of line" $
            "-- some very simple comment\nidentifier"
            `shouldParseTo`
            [TkComment, TkSpace, TkIdentifier]

        it "should allow endline escaping" $
            "-- first line\\\nsecond line\\\nand another one"
            `shouldParseTo`
            [TkComment]

    context "when parsing multi-line comments" $ do

        it "should support nested comments" $
            "{- comment {- nested -} still comment -} {- next comment -}"
            `shouldParseTo`
            [TkComment, TkSpace, TkComment]

        it "should distinguish compiler pragma" $
            "{- comment -}{-# LANGUAGE GADTs #-}{- comment -}"
            `shouldParseTo`
            [TkComment, TkPragma, TkComment]

    it "should recognize preprocessor directives" $ do
        "\n#define foo bar" `shouldParseTo` [TkSpace, TkCpp]
        "x # y" `shouldParseTo`
            [TkIdentifier, TkSpace, TkCpp, TkSpace,TkIdentifier]


shouldParseTo :: String -> [TokenType] -> Expectation
str `shouldParseTo` tokens = map tkType (parse str) `shouldBe` tokens
