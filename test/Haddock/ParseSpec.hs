{-# LANGUAGE OverloadedStrings, StandaloneDeriving
             , FlexibleInstances, UndecidableInstances
             , IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haddock.ParseSpec (main, spec) where

import           Control.Applicative
import           Data.Monoid
import           Data.String
import           Haddock.Doc (combineStringNodes)
import           Haddock.Lex (tokenise)
import qualified Haddock.Parse as Parse
import           Haddock.Types
import           Outputable (Outputable, showSDoc, ppr)
import           RdrName (RdrName)
import           Test.Hspec

import           Helper

instance Outputable a => Show a where
  show = showSDoc dynFlags . ppr

deriving instance Show a => Show (Doc a)
deriving instance Eq a => Eq (Doc a)


instance IsString (Doc RdrName) where
  fromString = DocString

instance IsString a => IsString (Maybe a) where
  fromString = Just . fromString

parseParas :: String -> Maybe (Doc RdrName)
parseParas s = Parse.parseParas $ tokenise dynFlags s (0,0)

parseString :: String -> Maybe (Doc RdrName)
parseString s = Parse.parseString $ tokenise dynFlags s (0,0)

main :: IO ()
main = hspec spec

infix 1 `shouldParseTo`
shouldParseTo :: String -> Doc RdrName -> Expectation
shouldParseTo input ast = (combineStringNodes <$> parseParas input)
                          `shouldBe` Just ast

spec :: Spec
spec = do
  describe "parseParas" $ do
    it "parses a paragraph" $ do
      "foobar" `shouldParseTo` DocParagraph "foobar\n"

    context "when parsing a simple string" $ do
      it "] should be made into a DocString" $ do
        "hell]o" `shouldParseTo` DocParagraph "hell]o\n"

      it "can handle unicode" $ do
        "灼眼のシャナ" `shouldParseTo` DocParagraph "灼眼のシャナ\n"

    context "when parsing module strings" $ do
      it "should parse a module on its own" $ do
        "\"Module\"" `shouldParseTo`
          (DocParagraph $ DocModule "Module" <> "\n")

      it "should parse a module inline" $ do
        "This is a \"Module\"." `shouldParseTo`
          DocParagraph ("This is a " <> ((DocModule "Module") <> ".\n"))

    context "when parsing emphasised strings" $ do
      it "emphasises a word on its own" $ do
        "/quux/" `shouldParseTo` (DocParagraph $ DocEmphasis "quux" <> "\n")

      it "emphasises inline correctly" $ do
        "This comment applies to the /following/ declaration" `shouldParseTo`
          (DocParagraph $ "This comment applies to the "
                <> DocEmphasis "following" <> " declaration\n")

      it "emphasises unicode" $ do
        "/灼眼のシャナ/" `shouldParseTo`
          (DocParagraph $ DocEmphasis "灼眼のシャナ" <> "\n")

      it "does /multi-line\\n codeblocks/" $ do
        " /multi-line\n emphasis/" `shouldParseTo`
          DocParagraph "/multi-line\n emphasis/\n"

    context "when parsing codeblocks" $ do
      it "codeblock a word on its own" $ do
        "@quux@" `shouldParseTo` DocCodeBlock "quux"

      it "codeblocks unicode" $ do
        "@灼眼のシャナ@" `shouldParseTo` DocCodeBlock "灼眼のシャナ"


      it "does @multi-line\\n codeblocks@" $ do
        "@multi-line\n codeblocks@" `shouldParseTo`
          DocCodeBlock "multi-line\n codeblocks"

      it "accepts other elements in a codeblock" $ do
        "@/emphasis/ \"Module\" <<picture>>@" `shouldParseTo`
          (DocCodeBlock $ DocEmphasis "emphasis" <> " "
                <> DocModule "Module" <> " " <> DocPic "picture")

    context "when parsing monospaced strings" $ do
      it "monospaces inline strings" $ do
        "This comment applies to the @following@ declaration" `shouldParseTo`
          (DocParagraph $ "This comment applies to the "
                <> DocMonospaced "following" <> " declaration\n")

      it "monospaces inline unicode" $ do
        "hello @灼眼のシャナ@ unicode" `shouldParseTo`
          (DocParagraph $ "hello "
                <> DocMonospaced "灼眼のシャナ" <> " unicode\n")

      it "accepts other elements in a monospaced section" $ do
        "hey @/emphasis/ \"Module\" <<picture>>@ world" `shouldParseTo`
          (DocParagraph $
               "hey "
            <> DocMonospaced (DocEmphasis "emphasis" <> " "
                              <> DocModule "Module" <> " " <> DocPic "picture")
            <> " world\n")


    context "when parsing unordered lists" $ do
      it "parses a simple unordered list" $ do
        "* point one\n\n* point two" `shouldParseTo`
          DocUnorderedList [ DocParagraph " point one\n"
                                 , DocParagraph " point two\n"]

      it "parses an empty unordered list" $ do
        "*" `shouldParseTo` DocUnorderedList [DocParagraph "\n"]

      it "accepts unicode in an unordered list" $ do
        "* 灼眼のシャナ" `shouldParseTo`
          DocUnorderedList [DocParagraph " 灼眼のシャナ\n"]

      it "accepts other elements in an unordered list" $ do
        ("* \"Module\"\n\n* /emphasis/"
                    ++ "\n\n* @code@\n\n* a@mono@b \n\n*") `shouldParseTo`
          DocUnorderedList [
              DocParagraph (" " <> DocModule "Module" <> "\n")
            , DocParagraph (" " <> DocEmphasis "emphasis" <> "\n")
            , DocCodeBlock "code"
            , DocParagraph (" a" <> (DocMonospaced "mono") <> "b \n")
            , DocParagraph "\n"
            ]

    context "when parsing ordered lists" $ do
      it "parses a simple ordered list" $ do
        "1. point one\n\n2. point two" `shouldParseTo`
          DocOrderedList [
              DocParagraph " point one\n"
            , DocParagraph " point two\n"
            ]

      it "parses an empty list" $ do
        "1." `shouldParseTo` DocOrderedList [DocParagraph "\n"]

        "(1)" `shouldParseTo` DocOrderedList [DocParagraph "\n"]

      it "accepts unicode" $ do
        "1. 灼眼のシャナ" `shouldParseTo`
          DocOrderedList [DocParagraph " 灼眼のシャナ\n"]

        "(1) 灼眼のシャナ" `shouldParseTo`
          DocOrderedList [DocParagraph " 灼眼のシャナ\n"]

      it "accepts other elements" $ do
        ("1. \"Module\"\n\n2. /emphasis/"
                    ++ "\n\n3. @code@\n\n4. a@mono@b \n\n5.") `shouldParseTo`
          DocOrderedList [
              DocParagraph (" " <> DocModule "Module" <> "\n")
            , DocParagraph (" " <> DocEmphasis "emphasis" <> "\n")
            , DocCodeBlock "code"
            , DocParagraph (" a" <> DocMonospaced "mono" <> "b \n")
            , DocParagraph "\n"
            ]

    context "when parsing definition lists" $ do
      it "parses a simple list" $ do
        "[foo] bar\n\n[baz] quux" `shouldParseTo`
          DocDefList [("foo", " bar\n"), ("baz", " quux\n")]

      it "parses a list with unicode in it" $ do
        "[灼眼] シャナ" `shouldParseTo`
          DocDefList [("灼眼", " シャナ\n")]

      it "parse other markup inside of it as usual" $ do
        "[/foo/] bar" `shouldParseTo`
          DocDefList [(DocEmphasis "foo", " bar\n")]

      it "doesn't need a string to follow it" $ do
        "[hello /world/]" `shouldParseTo`
          DocDefList [("hello " <> DocEmphasis "world", "\n")]

      it "takes input until the very last delimiter on the line" $ do
        "[[world]] bar" `shouldParseTo`
          DocDefList [("[world", "] bar\n")]

    context "when parsing an example" $ do
      it ("requires an example to be separated"
          ++ " from a previous paragraph by an empty line") $ do
        "foobar\n\n>>> fib 10\n55" `shouldParseTo`
          DocParagraph "foobar\n"
                <> DocExamples [Example "fib 10" ["55"]]

        -- parse error
        parseParas "foobar\n>>> fib 10\n55" `shouldBe` Nothing

      it "parses a prompt with no example results" $ do
        " >>> import Data.Char\n " `shouldParseTo`
          DocExamples [ Example { exampleExpression = "import Data.Char"
                                      , exampleResult = []
                                      }
                            ]

      it "is able to parse example sections with unicode" $ do
        " >>> 灼眼\n の\n >>> シャナ\n 封絶" `shouldParseTo`
          DocExamples [ Example { exampleExpression = "灼眼"
                                      , exampleResult = ["の"]
                                      }
                            , Example { exampleExpression = "シャナ"
                                      , exampleResult = ["封絶"]
                                      }
                            ]

      it ("parses a result line that only "
          ++ "contains <BLANKLINE> as an empty line") $ do
        ">>> putFooBar\nfoo\n<BLANKLINE>\nbar" `shouldParseTo`
          DocExamples [Example "putFooBar" ["foo","","bar"]]

    context "when parsing a code block" $ do
      it ("requires a code blocks to be "
          ++ "separated from a previous paragraph by an empty line") $ do
        "foobar\n\n> some code" `shouldParseTo`
          DocParagraph "foobar\n" <> DocCodeBlock " some code\n"

        -- parse error
        parseParas "foobar\n> some code" `shouldBe` Nothing

      it "consecutive birdtracks " $ do
        ">test3\n>test4\n\n" `shouldParseTo` DocCodeBlock "test3\ntest4\n"

      it "consecutive birdtracks with spaces " $ do
        " > foo\n \n > bar\n \n" `shouldParseTo`
          DocCodeBlock " foo\n" <> DocCodeBlock " bar\n"

      it "code block + birdtracks" $ do
        "@\ntest1\ntest2\n@\n\n>test3\n>test4\n\n" `shouldParseTo`
          DocCodeBlock "\ntest1\ntest2\n" <> DocCodeBlock "test3\ntest4\n"

      it "birdtracks + code block" $ do
        ">test3\n>test4\n\n@\ntest1\ntest2\n@\n\n" `shouldParseTo`
          DocCodeBlock "test3\ntest4\n" <> DocCodeBlock "\ntest1\ntest2\n"



      it "can parse consecutive prompts with results" $ do
        " >>> fib 5\n 5\n >>> fib 10\n 55" `shouldParseTo`
          DocExamples [ Example { exampleExpression = "fib 5"
                                      , exampleResult = ["5"]
                                      }
                            , Example { exampleExpression = "fib 10"
                                      , exampleResult = ["55"]
                                      }
                            ]

    context "when parsing a URL" $ do
      it "parses a URL" $ do
        "<http://example.com/>" `shouldParseTo`
          (DocParagraph $ hyperlink "http://example.com/" Nothing <> "\n")

      it "accepts an optional label" $ do
        "<http://example.com/ some link>" `shouldParseTo`
          (DocParagraph $ hyperlink "http://example.com/" "some link" <> "\n")

      it "consecutive URL and URL + label" $ do
        (" \nA plain URL: <http://example.com/>\n\n A URL with a "
                    ++ "label: <http://example.com/ some link>") `shouldParseTo`
          DocParagraph (
            "A plain URL: " <>
              DocHyperlink (Hyperlink "http://example.com/" Nothing) <> "\n"
          ) <>
          DocParagraph (
            "A URL with a label: " <>
              DocHyperlink (Hyperlink "http://example.com/" "some link") <> "\n"
          )

      it "finishes URL parsing as soon as it sees >, even if it's escaped" $ do
        "<http://examp\\>le.com" `shouldParseTo`
          DocParagraph (
            DocHyperlink (Hyperlink "http://examp\\" Nothing) <> "le.com\n"
          )

        "<http://exa\\>mp\\>le.com>" `shouldParseTo`
          DocParagraph (
            DocHyperlink (Hyperlink "http://exa\\" Nothing) <> "mp>le.com>\n"
          )

        -- Likewise in label
        "<http://example.com f\\>oo>" `shouldParseTo`
          DocParagraph (
            DocHyperlink (Hyperlink "http://example.com" "f\\") <> "oo>\n"
          )

      it "parses inline URLs" $ do
        (" Not yet working, see <http://trac.haskell.org"
                    ++ "/haddock/ticket/223>\n , isEmptyChan") `shouldParseTo`
          DocParagraph
                ("Not yet working, see "
                 <> ((DocHyperlink
                      (Hyperlink { hyperlinkUrl = "http://trac.haskell.org"
                                                  ++ "/haddock/ticket/223"
                                 , hyperlinkLabel = Nothing
                                 })) <> "\n , isEmptyChan\n"))

    context "when parsing properties" $ do
      it "can parse a single property" $ do
        "prop> 23 == 23" `shouldParseTo` DocProperty "23 == 23"

      it "can parse multiple subsequent properties" $ do
        unlines [
              "prop> 23 == 23"
            , "prop> 42 == 42"
            ]
        `shouldParseTo`
          DocProperty "23 == 23" <> DocProperty "42 == 42"

      it "accepts unicode in properties" $ do
        "prop> 灼眼のシャナ ≡ 愛" `shouldParseTo`
          DocProperty "灼眼のシャナ ≡ 愛"

      it "can deal with whitespace before and after the prop> prompt" $ do
        "  prop>     xs == (reverse $ reverse xs)" `shouldParseTo`
          DocProperty "xs == (reverse $ reverse xs)"

    context "when escaping elements" $ do

      it "escapes \\#\\#\\#" $ do
        " We should be able to escape this: \\#\\#\\#" `shouldParseTo`
          DocParagraph "We should be able to escape this: ###\n"

      it "escapes forward slashes" $ do
        " Existential \\/ Universal types" `shouldParseTo`
          DocParagraph "Existential / Universal types\n"

    context "when parsing pictures" $ do
      it "parses a simple picture" $ do
        "<<baz>>" `shouldParseTo`
          DocParagraph ((DocPic "baz") <> "\n")

      it "parses a picture with spaces" $ do
        "<<b a z>>" `shouldParseTo`
          DocParagraph ((DocPic "b a z") <> "\n")

      it "parses a picture with unicode" $ do
        "<<灼眼のシャナ>>" `shouldParseTo`
          DocParagraph ((DocPic "灼眼のシャナ") <> "\n")

      it "doesn't allow for escaping of the closing tags" $ do -- bug?
        "<<ba\\>>z>>" `shouldParseTo`
          (DocParagraph $ DocPic "ba\\" <> "z>>\n")

    context "when parsing anchors" $ do
      it "should parse a single word anchor" $ do
        "#foo#" `shouldParseTo`
          DocParagraph ((DocAName "foo") <> "\n")

      it "should parse a multi word anchor" $ do
        "#foo bar#" `shouldParseTo`
          DocParagraph ((DocAName "foo bar") <> "\n")

      it "should parse a unicode anchor" $ do
        "#灼眼のシャナ#" `shouldParseTo`
          DocParagraph (DocAName "灼眼のシャナ" <> "\n")

    context "replicates parsing of weird strings" $ do
      it "#f\\noo#" $ do
        "#f\noo#" `shouldParseTo` DocParagraph "#f\noo#\n"

      it "<b\\nar>" $ do
        "<b\nar>" `shouldParseTo` DocParagraph "<b\nar>\n"

      it "<<ba\\nz aar>>" $ do
        "<<ba\nz aar>>" `shouldParseTo` DocParagraph "<<ba\nz aar>>\n"

      it "[@q/uu/x@] h\\ney" $ do
        "[@q/uu/x@] h\ney" `shouldParseTo`
          DocDefList
                [(DocMonospaced
                  ((DocString "q")
                   <> ((DocEmphasis (DocString "uu"))
                       <> "x")), " h\ney\n")]

      it "[qu\\nx] hey" $ do
        parseParas "[qu\nx] hey" `shouldBe` Nothing

      it "/qu\\nux/" $ do
        "/qu\nux/" `shouldParseTo` DocParagraph "/qu\nux/\n"

    context "when parsing strings with apostrophes" $ do
      it "parses a word with an one of the delimiters in it as DocString" $ do
        "don't" `shouldParseTo` DocParagraph "don't\n"

      it "doesn't pass pairs of delimiters with spaces between them" $ do
        "hel'lo w'orld" `shouldParseTo` DocParagraph "hel'lo w'orld\n"

      it "don't use apostrophe's in the wrong place's" $ do
        " don't use apostrophe's in the wrong place's" `shouldParseTo`
          DocParagraph "don't use apostrophe's in the wrong place's\n"

  where
    hyperlink :: String -> Maybe String -> Doc RdrName
    hyperlink url = DocHyperlink . Hyperlink url
