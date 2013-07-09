{-# LANGUAGE OverloadedStrings, StandaloneDeriving
             , FlexibleInstances, UndecidableInstances
             , IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haddock.ParseSpec (main, spec) where

import           Control.Applicative
import           Data.Maybe (isJust)
import           Data.Monoid
import           Data.String
import           Haddock.Doc (combineStringNodes)
import qualified Haddock.Parser as Parse
import           Haddock.Types
import           Outputable (Outputable, showSDoc, ppr)
import           RdrName (RdrName)
import           Test.Hspec
import           Test.QuickCheck (property)

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
parseParas = Parse.parseParas dynFlags

parseString :: String -> Maybe (Doc RdrName)
parseString = Parse.parseString dynFlags

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let filterSpecial = filter (`notElem` (".(=#-[*`\v\f\n\t\r\\\"'_/@<> " :: String))

  describe "parseString" $ do
    let infix 1 `shouldParseTo`
        shouldParseTo :: String -> Doc RdrName -> Expectation
        shouldParseTo input ast = parseString input `shouldBe` Just ast

    it "is total" $ do
      property $ \xs ->
        -- filter out primes as we might end up with an identifier
        -- which will fail due to undefined DynFlags
        parseString (filter (/= '\'') xs) `shouldSatisfy` isJust

    context "when parsing URLs" $ do
      it "parses a URL" $ do
        "<http://example.com/>" `shouldParseTo`
          hyperlink "http://example.com/" Nothing <> "\n"

      it "accepts an optional label" $ do
        "<http://example.com/ some link>" `shouldParseTo`
          hyperlink "http://example.com/" "some link" <> "\n"

      it "finishes URL parsing as soon as it sees >, even if it's escaped" $ do
        "<http://examp\\>le.com" `shouldParseTo`
          hyperlink "http://examp\\" Nothing <> "le.com\n"

        "<http://exa\\>mp\\>le.com>" `shouldParseTo`
          hyperlink "http://exa\\" Nothing <> "mp>le.com>\n"

        -- Likewise in label
        "<http://example.com f\\>oo>" `shouldParseTo`
          hyperlink "http://example.com" "f\\" <> "oo>\n"

      it "parses inline URLs" $ do
        "Not yet working, see <http://trac.haskell.org/haddock/ticket/223>\n , isEmptyChan" `shouldParseTo`
             "Not yet working, see "
          <> hyperlink "http://trac.haskell.org/haddock/ticket/223" Nothing
          <> "\n , isEmptyChan\n"

      context "when autolinking URLs" $ do
        it "autolinks HTTP URLs" $ do
          "http://example.com/" `shouldParseTo`
            hyperlink "http://example.com/" Nothing <> "\n"

        it "autolinks HTTPS URLs" $ do
          "https://www.example.com/" `shouldParseTo`
            hyperlink "https://www.example.com/" Nothing <> "\n"

        it "autolinks FTP URLs" $ do
          "ftp://example.com/" `shouldParseTo`
            hyperlink "ftp://example.com/" Nothing <> "\n"

        it "does not include a trailing exclamation mark" $ do
          "http://example.com/! Some other sentence." `shouldParseTo`
            hyperlink "http://example.com/" Nothing <> "! Some other sentence.\n"

        it "does not include a trailing comma" $ do
          "http://example.com/, Some other sentence." `shouldParseTo`
            hyperlink "http://example.com/" Nothing <> ", Some other sentence.\n"

        it "does not include a trailing dot" $ do
          "http://example.com/. Some other sentence." `shouldParseTo`
            hyperlink "http://example.com/" Nothing <> ". Some other sentence.\n"

        it "does not include a trailing question mark" $ do
          "http://example.com/? Some other sentence." `shouldParseTo`
            hyperlink "http://example.com/" Nothing <> "? Some other sentence.\n"


  describe "parseParas" $ do
    let infix 1 `shouldParseTo`
        shouldParseTo :: String -> Doc RdrName -> Expectation
        shouldParseTo input ast = (combineStringNodes <$> parseParas input)
                                  `shouldBe` Just ast

    it "is total" $ do
      property $ \xs ->
        -- filter out primes as we might end up with an identifier
        -- which will fail due to undefined DynFlags
        parseParas (filter (/= '\'') xs) `shouldSatisfy` isJust

    it "parses a paragraph" $ do
      "foobar" `shouldParseTo` DocParagraph "foobar\n"

    it "empty input produces DocEmpty" $ do
      "" `shouldParseTo` DocEmpty

    it "should preserve all regular characters" $ do
      property $ \xs ->
        let input = filterSpecial xs
        in case input of
          [] -> input `shouldParseTo` DocEmpty
          _ -> input `shouldParseTo` DocParagraph (DocString $ input ++ "\n")

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
          DocParagraph ("This is a " <> (DocModule "Module" <> ".\n"))

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

      it "does not do /multi-line\\n emphasis/" $ do
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
                <> DocModule "Module" <> " " <> pic "picture" Nothing)

    context "when parsing monospaced strings" $ do
      it "monospaces inline strings" $ do
        "This comment applies to the @following@ declaration" `shouldParseTo`
          (DocParagraph $ "This comment applies to the "
                <> DocMonospaced "following" <> " declaration\n")

      it "should allow us to escape the @" $ do
        "foo @hey \\@ world@ bar" `shouldParseTo`
          DocParagraph ("foo " <> DocMonospaced "hey @ world" <> " bar\n")

      it "monospaces inline unicode" $ do
        "hello @灼眼のシャナ@ unicode" `shouldParseTo`
          (DocParagraph $ "hello "
                <> DocMonospaced "灼眼のシャナ" <> " unicode\n")

      it "accepts other elements in a monospaced section" $ do
        "hey @/emphasis/ \"Module\" <<picture>>@ world" `shouldParseTo`
          (DocParagraph $
           "hey "
           <> DocMonospaced (DocEmphasis "emphasis" <> " "
                             <> DocModule "Module" <> " " <> pic "picture" Nothing)
           <> " world\n")


    context "when parsing unordered lists" $ do
      it "parses a simple unordered list" $ do
        "* point one\n\n* point two" `shouldParseTo`
          DocUnorderedList [ DocParagraph " point one\n"
                           , DocParagraph " point two\n"]

        "* 1.parameter re :  the derived regular expression"
          ++ "\n\n- returns : empty String" `shouldParseTo`
          (DocUnorderedList
           [DocParagraph " 1.parameter re :  the derived regular expression\n",
            DocParagraph " returns : empty String\n"])

      it "doesn't accept a list where unexpected" $ do
        " expression?\n -> matches\n\n    * 1.parameter \n\n"
          `shouldParseTo`
           DocParagraph "expression?\n -> matches\n" <> DocUnorderedList [DocParagraph " 1.parameter \n"]


      it "parses a simple unordered list without the empty line separator" $ do
        "* point one\n* point two" `shouldParseTo`
          DocUnorderedList [ DocParagraph " point one\n"
                           , DocParagraph " point two\n"]

        "* point one\nmore one\n* point two\nmore two" `shouldParseTo`
          DocUnorderedList [ DocParagraph " point one\nmore one\n"
                           , DocParagraph " point two\nmore two\n"]

        " * point one\nmore one\n * point two\nmore two" `shouldParseTo`
          DocUnorderedList [ DocParagraph " point one\nmore one\n"
                           , DocParagraph " point two\nmore two\n"
                           ]

      it "parses an empty unordered list" $ do
        "*" `shouldParseTo` DocUnorderedList [DocParagraph "\n"]

      it "accepts unicode in an unordered list" $ do
        "* 灼眼のシャナ" `shouldParseTo`
          DocUnorderedList [DocParagraph " 灼眼のシャナ\n"]

      it "preserves whitespace on the front of additional lines" $ do
        "* foo\n    bar" `shouldParseTo` DocUnorderedList [DocParagraph " foo\n    bar\n"]

      it "accepts other elements in an unordered list" $ do
        ("* \"Module\"\n\n* /emphasis/"
                    ++ "\n\n* @code@\n\n* a@mono@b \n\n*") `shouldParseTo`
          DocUnorderedList [
              DocParagraph (" " <> DocModule "Module" <> "\n")
            , DocParagraph (" " <> DocEmphasis "emphasis" <> "\n")
            , DocCodeBlock "code"
            , DocParagraph (" a" <> DocMonospaced "mono" <> "b \n")
            , DocParagraph "\n"
            ]

        ("* \"Module\"\n* /emphasis/"
                    ++ "\n* @code@\n* a@mono@b \n*") `shouldParseTo`
          DocUnorderedList [
              DocParagraph (" " <> DocModule "Module" <> "\n")
            , DocParagraph (" " <> DocEmphasis "emphasis" <> "\n")
            , DocCodeBlock "code"
            , DocParagraph (" a" <> DocMonospaced "mono" <> "b \n")
            , DocParagraph "\n"
            ]

    context "when parsing ordered lists" $ do
      it "parses a simple ordered list" $ do
        "1. point one\n\n2. point two" `shouldParseTo`
          DocOrderedList [ DocParagraph " point one\n"
                         , DocParagraph " point two\n"
                         ]

      it "parses a simple ordered list without the newline separator" $ do
        "1. point one\n2. point two" `shouldParseTo`
          DocOrderedList [ DocParagraph " point one\n"
                         , DocParagraph " point two\n"
                         ]

        "1. point one\nmore\n2. point two\nmore" `shouldParseTo`
          DocOrderedList [ DocParagraph " point one\nmore\n"
                         , DocParagraph " point two\nmore\n"
                         ]

        -- space before list
        " 1. point one\nmore\n 2. point two\nmore" `shouldParseTo`
          DocOrderedList [ DocParagraph " point one\nmore\n"
                         , DocParagraph " point two\nmore\n"
                         ]

      it "parses an empty list" $ do
        "1." `shouldParseTo` DocOrderedList [DocParagraph "\n"]

        "(1)" `shouldParseTo` DocOrderedList [DocParagraph "\n"]

      it "accepts unicode" $ do
        "1. 灼眼のシャナ" `shouldParseTo`
          DocOrderedList [DocParagraph " 灼眼のシャナ\n"]

        "(1) 灼眼のシャナ" `shouldParseTo`
          DocOrderedList [DocParagraph " 灼眼のシャナ\n"]

      it "preserves whitespace on the front of additional lines" $ do
        "1. foo\n    bar" `shouldParseTo` DocOrderedList [DocParagraph " foo\n    bar\n"]

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

      it "parses a simple list without the newline separator" $ do
        "[foo] bar\n[baz] quux" `shouldParseTo`
          DocDefList [("foo", " bar\n"), ("baz", " quux\n")]

        "[foo] bar\nmore\n[baz] quux\nmore" `shouldParseTo`
          DocDefList [("foo", " bar\nmore\n"), ("baz", " quux\nmore\n")]

        " [foo] bar\nmore\n [baz] quux\nmore" `shouldParseTo`
          DocDefList [("foo", " bar\nmore\n"), ("baz", " quux\nmore\n")]

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

      it "treats broken up definition list as regular string" $ do
        "[qu\nx] hey" `shouldParseTo` DocParagraph "[qu\nx] hey\n"

      it "preserves whitespace on the front of additional lines" $ do
        "[foo] bar\n    baz" `shouldParseTo` DocDefList [("foo", " bar\n    baz\n")]

    context "when parsing consecutive paragraphs" $ do
      it "will not capture irrelevant consecutive lists" $ do
        "   * bullet\n\n   - different bullet\n\n   (1) ordered\n \n   "
          ++ "2. different bullet\n   \n   [cat] kitten\n   \n   [pineapple] fruit"
          `shouldParseTo`
          DocUnorderedList [ DocParagraph " bullet\n"
                           , DocParagraph " different bullet\n"]
          <> DocOrderedList [ DocParagraph " ordered\n"
                            , DocParagraph " different bullet\n"
                            ]
          <> DocDefList [ ("cat", " kitten\n")
                        , ("pineapple", " fruit\n")
                        ]

    context "when parsing an example" $ do
      it ("requires an example to be separated"
          ++ " from a previous paragraph by an empty line") $ do
        "foobar\n\n>>> fib 10\n55" `shouldParseTo`
          DocParagraph "foobar\n"
                <> DocExamples [Example "fib 10" ["55"]]

        -- parse error
      it "parses bird-tracks inside of paragraphs as plain strings" $ do
        "foobar\n>>> fib 10\n55" `shouldParseTo` DocParagraph "foobar\n>>> fib 10\n55\n"

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
      it "preserves whitespace before the prompt with consecutive paragraphs" $ do
        " Examples:\n\n >>> fib 5\n 5\n >>> fib 10\n 55\n\n >>> fib 10\n 55"
        `shouldParseTo`
        DocParagraph "Examples:\n"
          <> DocExamples [ Example { exampleExpression = "fib 5"
                                   , exampleResult = ["5"]}
                         , Example {exampleExpression = "fib 10"
                                   , exampleResult = ["55"]}]
          <> DocExamples [ Example { exampleExpression = "fib 10"
                                   , exampleResult = ["55"]}]

      it "can parse consecutive prompts with results" $ do
        " >>> fib 5\n 5\n >>> fib 10\n 55" `shouldParseTo`
          DocExamples [ Example { exampleExpression = "fib 5"
                                , exampleResult = ["5"] }
                      , Example { exampleExpression = "fib 10"
                                , exampleResult = ["55"] }]

      it "can parse results if they don't have the same whitespace prefix" $ do
        " >>> hey\n5\n 5\n  5" `shouldParseTo`
          DocExamples [ Example { exampleExpression = "hey"
                                , exampleResult = ["5", "5", " 5"] }]


      it "parses a <BLANKLINE> result as an empty result" $ do
        ">>> putFooBar\nfoo\n<BLANKLINE>\nbar" `shouldParseTo`
          DocExamples [Example "putFooBar" ["foo","","bar"]]

    context "when parsing a code block" $ do
      it ("requires a code blocks to be "
          ++ "separated from a previous paragraph by an empty line") $ do
        "foobar\n\n> some code" `shouldParseTo`
          DocParagraph "foobar\n" <> DocCodeBlock " some code\n"

      it "parses birdtracks inside of paragraphs as plain strings" $ do
        "foobar\n> some code" `shouldParseTo` DocParagraph "foobar\n> some code\n"

      it "long birdtrack block without spaces in front" $ do
        "beginning\n\n> foo\n> bar\n> baz" `shouldParseTo`
          DocParagraph "beginning\n"
          <> DocCodeBlock " foo\n bar\n baz\n"

      it "single DocCodeBlock even if there's space before birdtracks" $ do
        "beginning\n\n > foo\n > bar\n > baz" `shouldParseTo`
          DocParagraph "beginning\n"
          <> DocCodeBlock " foo\n bar\n baz\n"

      it "consecutive birdtracks with spaces " $ do
        " > foo\n \n > bar\n \n" `shouldParseTo`
          DocCodeBlock " foo\n" <> DocCodeBlock " bar\n"

      it "code block + birdtracks" $ do
        "@\ntest1\ntest2\n@\n\n>test3\n>test4\n\n" `shouldParseTo`
          DocCodeBlock "\ntest1\ntest2\n"
          <> DocCodeBlock "test3\ntest4\n"

      it "requires the code block to be closed" $ do
        "@hello" `shouldParseTo` DocParagraph "@hello\n"

      it "preserves the first trailing whitespace after the opening @ in a code block" $ do
        "@\ntest1\ntest2\n@" `shouldParseTo` DocCodeBlock "\ntest1\ntest2\n"

        "@   \ntest1\ntest2\n@" `shouldParseTo` DocCodeBlock "   \ntest1\ntest2\n"

      it "markup in a @ code block" $ do
        "@hello <world> \"Foo.Bar\" <<how is>> it /going/?@" `shouldParseTo`
          DocCodeBlock
          ("hello " <>
           (DocHyperlink (Hyperlink {hyperlinkUrl = "world", hyperlinkLabel = Nothing}))
           <> " "
           <> DocModule "Foo.Bar"
           <> " "
           <> (DocPic (Picture {pictureUri = "how", pictureTitle = Just "is"}))
           <> " it " <> (DocEmphasis "going")
           <> "?")

      it "should allow us to escape the @ in a paragraph level @ code block" $ do
        "@hello \\@ world@" `shouldParseTo` DocCodeBlock "hello @ world"

      it "should swallow up trailing spaces in code blocks" $ do
        "@ foo @" `shouldParseTo` DocCodeBlock " foo"

      it "birdtracks + code block" $ do
        ">test3\n>test4\n\n@\ntest1\ntest2\n@\n\n" `shouldParseTo`
          DocCodeBlock "test3\ntest4\n"
          <> DocCodeBlock "\ntest1\ntest2\n"

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
          DocParagraph (pic "baz" Nothing <> "\n")

      it "parses a picture with a title" $ do
        "<<b a z>>" `shouldParseTo`
          DocParagraph (pic "b" (Just "a z") <> "\n")

      it "parses a picture with unicode" $ do
        "<<灼眼のシャナ>>" `shouldParseTo`
          DocParagraph ((pic "灼眼のシャナ" Nothing) <> "\n")

      it "doesn't allow for escaping of the closing tags" $ do -- bug?
        "<<ba\\>>z>>" `shouldParseTo`
          (DocParagraph $ pic "ba\\" Nothing <> "z>>\n")

    context "when parsing anchors" $ do
      it "should parse a single word anchor" $ do
        "#foo#" `shouldParseTo`
          DocParagraph (DocAName "foo" <> "\n")

      it "should parse a multi word anchor" $ do
        "#foo bar#" `shouldParseTo`
          DocParagraph (DocAName "foo bar" <> "\n")

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
                [(DocMonospaced ("q" <> DocEmphasis "uu" <> "x"), " h\ney\n")]

      it "/qu\\nux/" $ do
        "/qu\nux/" `shouldParseTo` DocParagraph "/qu\nux/\n"

      -- regression test
      it "requires markup to be fully closed, even if nested" $ do
        "@hel/lo" `shouldParseTo` DocParagraph "@hel/lo\n"

      it "will be total even if only the first delimiter is present" $ do
        "/" `shouldParseTo` DocParagraph "/\n"

    context "when parsing strings with apostrophes" $ do
      it "parses a word with an one of the delimiters in it as DocString" $ do
        "don't" `shouldParseTo` DocParagraph "don't\n"

      it "doesn't pass pairs of delimiters with spaces between them" $ do
        "hel'lo w'orld" `shouldParseTo` DocParagraph "hel'lo w'orld\n"

      it "don't use apostrophe's in the wrong place's" $ do
        " don't use apostrophe's in the wrong place's" `shouldParseTo`
          DocParagraph "don't use apostrophe's in the wrong place's\n"

    context "when parsing strings contaning numeric character references" $ do
      it "will implicitly convert digits to characters" $ do
        "&#65;&#65;&#65;&#65;" `shouldParseTo` DocParagraph "AAAA\n"

        "&#28796;&#30524;&#12398;&#12471;&#12515;&#12490;" `shouldParseTo`
          DocParagraph "灼眼のシャナ\n"

      it "will implicitly convert hex encoded characters" $ do
        "&#x65;&#x65;&#x65;&#x65;" `shouldParseTo` DocParagraph "eeee\n"

    context "when parsing module names" $ do
      it "can accept a simple module name" $ do
        "\"Hello\"" `shouldParseTo` DocParagraph (DocModule "Hello" <> "\n")

      it "can accept a module name with dots" $ do
        "\"Hello.World\"" `shouldParseTo` DocParagraph (DocModule "Hello.World" <> "\n")

      it "can accept a module name with unicode" $ do
        "\"Hello.Worldλ\"" `shouldParseTo` DocParagraph ((DocModule "Hello.Worldλ") <> "\n")

      it "parses a module name with a trailing dot as regular quoted string" $ do
        "\"Hello.\"" `shouldParseTo` DocParagraph "\"Hello.\"\n"

      it "parses a module name with a space as regular quoted string" $ do
        "\"Hello World\"" `shouldParseTo` DocParagraph "\"Hello World\"\n"

      it "parses a module name with invalid characters as regular quoted string" $ do
        "\"Hello&[{}(=*)+]!\"" `shouldParseTo` DocParagraph "\"Hello&[{}(=*)+]!\"\n"


  where
    hyperlink :: String -> Maybe String -> Doc RdrName
    hyperlink url = DocHyperlink . Hyperlink url

    pic :: String -> Maybe String -> Doc RdrName
    pic uri = DocPic . Picture uri
