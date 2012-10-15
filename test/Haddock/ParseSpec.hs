{-# LANGUAGE OverloadedStrings, StandaloneDeriving, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haddock.ParseSpec (main, spec) where

import           Test.Hspec
import           RdrName (RdrName)
import           DynFlags (DynFlags, defaultDynFlags)
import           Haddock.Lex (tokenise)
import qualified Haddock.Parse as Parse
import           Haddock.Types
import           Outputable (Outputable, showSDoc, ppr)
import           Data.Monoid
import           Data.String

dynFlags :: DynFlags
dynFlags = defaultDynFlags (error "dynFlags for Haddock tests: undefined")

instance Outputable a => Show a where
  show = showSDoc dynFlags . ppr

deriving instance Show a => Show (Doc a)
deriving instance Eq a =>Eq (Doc a)

instance IsString (Doc RdrName) where
  fromString = DocString

parseParas :: String -> Maybe (Doc RdrName)
parseParas s = Parse.parseParas $ tokenise dynFlags s (0,0)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseParas" $ do
    it "parses a paragraph" $ do
      parseParas "foobar" `shouldBe` Just (DocParagraph "foobar\n")

    context "when parsing an example" $ do
      it "requires an example to be separated from a previous paragrap by an empty line" $ do
        parseParas "foobar\n\n>>> fib 10\n55" `shouldBe`
          Just (DocParagraph "foobar\n" <> DocExamples [Example "fib 10" ["55"]])

        -- parse error
        parseParas "foobar\n>>> fib 10\n55" `shouldBe` Nothing

      it "parses a result line that only contains <BLANKLINE> as an emptly line" $ do
        parseParas ">>> putFooBar\nfoo\n<BLANKLINE>\nbar" `shouldBe`
          Just (DocExamples [Example "putFooBar" ["foo","","bar"]])

    context "when parsing a code block" $ do
      it "requires a code blocks to be separated from a previous paragrap by an empty line" $ do
        parseParas "foobar\n\n> some code" `shouldBe`
          Just (DocParagraph "foobar\n" <> DocCodeBlock " some code\n")

        -- parse error
        parseParas "foobar\n> some code" `shouldBe` Nothing


    context "when parsing a URL" $ do
      it "parses a URL" $ do
        parseParas "<http://example.com/>" `shouldBe`
          Just (DocParagraph $ hyperlink "http://example.com/" Nothing <> "\n")

      it "accepts an optional label" $ do
        parseParas "<http://example.com/ some link>" `shouldBe`
          Just (DocParagraph $ hyperlink "http://example.com/" (Just "some link") <> "\n")

    context "when parsing properties" $ do
      it "can parse a single property" $ do
        parseParas "prop> 23 == 23" `shouldBe` Just (DocProperty "23 == 23")

      it "can parse multiple subsequent properties" $ do
        parseParas $ unlines [
              "prop> 23 == 23"
            , "prop> 42 == 42"
            ]
        `shouldBe` Just (DocProperty "23 == 23" <> DocProperty "42 == 42")
  where
    hyperlink :: String -> Maybe String -> Doc RdrName
    hyperlink url = DocHyperlink . Hyperlink url
