module Haddock.Backends.Hyperlinker.ParserSpec (main, spec) where


import Test.Hspec
import Test.QuickCheck

import qualified GHC
import Control.Monad.IO.Class

import Haddock (getGhcDirs)
import Haddock.Backends.Hyperlinker.Parser
import Haddock.Backends.Hyperlinker.Types

withDynFlags :: (GHC.DynFlags -> IO ()) -> IO ()
withDynFlags cont = do
  libDir <- fmap snd (getGhcDirs [])
  GHC.runGhc (Just libDir) $ do
    dflags <- GHC.getSessionDynFlags
    liftIO $ cont dflags


main :: IO ()
main = hspec spec


spec :: Spec
spec = describe "parse" parseSpec


-- | Defined for its instance of 'Arbitrary'
newtype NoTabs = NoTabs String deriving (Show, Eq)

noTabs :: String -> Bool
noTabs = all (\c -> c `notElem` "\r\t\f\v")

-- | Does not generate content with space characters other than ' ' and '\n'
instance Arbitrary NoTabs where
  arbitrary = fmap NoTabs (arbitrary `suchThat` noTabs)
  shrink (NoTabs src) = [ NoTabs shrunk | shrunk <- shrink src, noTabs shrunk ]


parseSpec :: Spec
parseSpec = around withDynFlags $ do

    it "is total" $ \dflags ->
        property $ \src -> length (parse dflags "" src) `shouldSatisfy` (>= 0)

    it "retains file layout" $ \dflags ->
        property $ \(NoTabs src) -> concatMap tkValue (parse dflags "" src) == src

    context "when parsing single-line comments" $ do

        it "should ignore content until the end of line" $ \dflags ->
            shouldParseTo
                "-- some very simple comment\nidentifier"
                [TkComment, TkSpace, TkIdentifier]
                dflags

        it "should allow endline escaping" $ \dflags ->
            shouldParseTo
                "#define first line\\\nsecond line\\\nand another one"
                [TkCpp]
                dflags

    context "when parsing multi-line comments" $ do

        it "should support nested comments" $ \dflags ->
            shouldParseTo
                "{- comment {- nested -} still comment -} {- next comment -}"
                [TkComment, TkSpace, TkComment]
                dflags

        it "should distinguish compiler pragma" $ \dflags ->
            shouldParseTo
                "{- comment -}{-# LANGUAGE GADTs #-}{- comment -}"
                [TkComment, TkPragma, TkComment]
                dflags

    it "should recognize preprocessor directives" $ \dflags -> do
            shouldParseTo
                "\n#define foo bar"
                [TkSpace, TkCpp]
                dflags
            shouldParseTo
                "x # y"
                [TkIdentifier, TkSpace, TkOperator, TkSpace,TkIdentifier]
                dflags

    it "should distinguish basic language constructs" $ \dflags -> do
        
        shouldParseTo
            "(* 2) <$> (\"abc\", foo)"
            [ TkSpecial, TkOperator, TkSpace, TkNumber, TkSpecial
            , TkSpace, TkOperator, TkSpace
            , TkSpecial, TkString, TkSpecial, TkSpace, TkIdentifier, TkSpecial
            ]
            dflags
            
        shouldParseTo
            "let foo' = foo in foo' + foo'"
            [ TkKeyword, TkSpace, TkIdentifier
            , TkSpace, TkGlyph, TkSpace
            , TkIdentifier, TkSpace, TkKeyword, TkSpace
            , TkIdentifier, TkSpace, TkOperator, TkSpace, TkIdentifier
            ]
            dflags
        
        shouldParseTo
            "square x = y^2 where y = x"
            [ TkIdentifier, TkSpace, TkIdentifier
            , TkSpace, TkGlyph, TkSpace
            , TkIdentifier, TkOperator, TkNumber
            , TkSpace, TkKeyword, TkSpace
            , TkIdentifier, TkSpace, TkGlyph, TkSpace, TkIdentifier
            ]
            dflags

    it "should parse do-notation syntax" $ \dflags -> do
        shouldParseTo
            "do { foo <- getLine; putStrLn foo }"
            [ TkKeyword, TkSpace, TkSpecial, TkSpace
            , TkIdentifier, TkSpace, TkGlyph, TkSpace
            , TkIdentifier, TkSpecial, TkSpace
            , TkIdentifier, TkSpace, TkIdentifier, TkSpace, TkSpecial
            ]
            dflags

        shouldParseTo
            (unlines
                [ "do"
                , "    foo <- getLine"
                , "    putStrLn foo"
                ])
            [ TkKeyword, TkSpace, TkIdentifier
            , TkSpace, TkGlyph, TkSpace, TkIdentifier, TkSpace
            , TkIdentifier, TkSpace, TkIdentifier, TkSpace
            ]
            dflags
  where
    shouldParseTo :: String -> [TokenType] -> GHC.DynFlags -> Expectation
    shouldParseTo str tokens dflags = map tkType (parse dflags "" str) `shouldBe` tokens
