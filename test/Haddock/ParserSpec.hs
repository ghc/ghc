{-# LANGUAGE OverloadedStrings, StandaloneDeriving
             , FlexibleInstances, UndecidableInstances
             , IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haddock.ParserSpec (main, spec) where

import           Data.Monoid
import           Data.String
import qualified Haddock.Parser as Parse
import           Haddock.Types
import           Outputable (Outputable, showSDoc, ppr)
import           RdrName (RdrName, mkVarUnqual)
import           FastString (fsLit)
import           StaticFlags (initStaticOpts)
import           Test.Hspec
import           Test.QuickCheck

import           Helper

instance Outputable a => Show a where
  show = showSDoc dynFlags . ppr

deriving instance Show a => Show (Header a)
deriving instance Show a => Show (Doc a)
deriving instance Eq a => Eq (Header a)
deriving instance Eq a => Eq (Doc a)

instance IsString RdrName where
  fromString = mkVarUnqual . fsLit

instance IsString (Doc RdrName) where
  fromString = DocString

instance IsString a => IsString (Maybe a) where
  fromString = Just . fromString

parseParas :: String -> Doc RdrName
parseParas = Parse.parseParas dynFlags

parseString :: String -> Doc RdrName
parseString = Parse.parseString dynFlags

main :: IO ()
main = hspec spec

spec :: Spec
spec = before initStaticOpts $ do
  describe "parseString" $ do
    let infix 1 `shouldParseTo`
        shouldParseTo :: String -> Doc RdrName -> Expectation
        shouldParseTo input ast = parseString input `shouldBe` ast

    it "is total" $ do
      property $ \xs ->
        (length . show . parseString) xs `shouldSatisfy` (> 0)

    context "when parsing text" $ do
      it "can handle unicode" $ do
        "灼眼のシャナ" `shouldParseTo` "灼眼のシャナ"

      it "accepts numeric character references" $ do
        "foo b&#97;r b&#97;z &#955;" `shouldParseTo` "foo bar baz λ"

      it "accepts hexadecimal character references" $ do
        "&#x65;" `shouldParseTo` "e"

      it "allows to backslash-escape characters" $ do
        property $ \x -> ['\\', x] `shouldParseTo` DocString [x]

    context "when parsing identifiers" $ do
      it "parses identifiers enclosed within single ticks" $ do
        "'foo'" `shouldParseTo` DocIdentifier "foo"

      it "parses identifiers enclosed within backticks" $ do
        "`foo`" `shouldParseTo` DocIdentifier "foo"

      it "parses a word with one of the delimiters in it as ordinary string" $ do
        "don't use apostrophe's in the wrong place's" `shouldParseTo` "don't use apostrophe's in the wrong place's"

    context "when parsing URLs" $ do
      let hyperlink :: String -> Maybe String -> Doc RdrName
          hyperlink url = DocHyperlink . Hyperlink url

      it "parses a URL" $ do
        "<http://example.com/>" `shouldParseTo` hyperlink "http://example.com/" Nothing

      it "accepts an optional label" $ do
        "<http://example.com/ some link>" `shouldParseTo` hyperlink "http://example.com/" "some link"

      it "does not accept newlines in label" $ do
        "<foo bar\nbaz>" `shouldParseTo` "<foo bar\nbaz>"

      it "does not allow to escap >" $ do
        "<http://examp\\>le.com" `shouldParseTo` hyperlink "http://examp\\" Nothing <> "le.com"

      it "parses inline URLs" $ do
        "foo <http://example.com/> bar" `shouldParseTo`
          "foo " <> hyperlink "http://example.com/" Nothing <> " bar"

      context "when autolinking URLs" $ do
        it "autolinks HTTP URLs" $ do
          "http://example.com/" `shouldParseTo` hyperlink "http://example.com/" Nothing

        it "autolinks HTTPS URLs" $ do
          "https://www.example.com/" `shouldParseTo` hyperlink "https://www.example.com/" Nothing

        it "autolinks FTP URLs" $ do
          "ftp://example.com/" `shouldParseTo` hyperlink "ftp://example.com/" Nothing

        it "does not include a trailing comma" $ do
          "http://example.com/, Some other sentence." `shouldParseTo`
            hyperlink "http://example.com/" Nothing <> ", Some other sentence."

        it "does not include a trailing dot" $ do
          "http://example.com/. Some other sentence." `shouldParseTo`
            hyperlink "http://example.com/" Nothing <> ". Some other sentence."

        it "does not include a trailing exclamation mark" $ do
          "http://example.com/! Some other sentence." `shouldParseTo`
            hyperlink "http://example.com/" Nothing <> "! Some other sentence."

        it "does not include a trailing question mark" $ do
          "http://example.com/? Some other sentence." `shouldParseTo`
            hyperlink "http://example.com/" Nothing <> "? Some other sentence."

    context "when parsing pictures" $ do
      let picture :: String -> Maybe String -> Doc RdrName
          picture uri = DocPic . Picture uri

      it "parses a simple picture" $ do
        "<<foo>>" `shouldParseTo` picture "foo" Nothing

      it "accepts an optional title" $ do
        "<<foo bar baz>>" `shouldParseTo` picture "foo" (Just "bar baz")

      it "does not accept newlines in title" $ do
        "<<foo bar\nbaz>>" `shouldParseTo` "<<foo bar\nbaz>>"

      it "parses a picture with unicode" $ do
        "<<灼眼 のシャナ>>" `shouldParseTo` picture "灼眼" (Just "のシャナ")

      it "doesn't allow for escaping of the closing tags" $ do -- bug?
        "<<ba\\>>z>>" `shouldParseTo` picture "ba\\" Nothing <> "z>>"

    context "when parsing anchors" $ do
      it "parses a single word anchor" $ do
        "#foo#" `shouldParseTo` DocAName "foo"

      it "parses a multi word anchor" $ do
        "#foo bar#" `shouldParseTo` DocAName "foo bar"

      it "parses a unicode anchor" $ do
        "#灼眼のシャナ#" `shouldParseTo` DocAName "灼眼のシャナ"

      it "does not accept newlines in anchors" $ do
        "#foo\nbar#" `shouldParseTo` "#foo\nbar#"

    context "when parsing emphasised text" $ do
      it "emphasises a word on its own" $ do
        "/foo/" `shouldParseTo` DocEmphasis "foo"

      it "emphasises inline correctly" $ do
        "foo /bar/ baz" `shouldParseTo` "foo " <> DocEmphasis "bar" <> " baz"

      it "emphasises unicode" $ do
        "/灼眼のシャナ/" `shouldParseTo` DocEmphasis "灼眼のシャナ"

      it "does not emphasise multi-line strings" $ do
        " /foo\nbar/" `shouldParseTo` "/foo\nbar/"

      it "does not emphasise the empty string" $ do
        "//" `shouldParseTo` "//"

      it "parses escaped slashes literally" $ do
        "/foo\\/bar/" `shouldParseTo` DocEmphasis "foo/bar"

      it "recognizes other markup constructs within emphasised text" $ do
        "/foo @bar@ baz/" `shouldParseTo`
          DocEmphasis ("foo " <> DocMonospaced "bar" <> " baz")

      it "allows other markup inside of emphasis" $ do
        "/__inner bold__/" `shouldParseTo` DocEmphasis (DocBold "inner bold")

      it "doesn't mangle inner markup unicode" $ do
        "/__灼眼のシャナ &#65;__/" `shouldParseTo` DocEmphasis (DocBold "灼眼のシャナ A")

      it "properly converts HTML escape sequences" $ do
        "/&#65;&#65;&#65;&#65;/" `shouldParseTo` DocEmphasis "AAAA"

      it "allows to escape the emphasis delimiter inside of emphasis" $ do
        "/empha\\/sis/" `shouldParseTo` DocEmphasis "empha/sis"

    context "when parsing bold strings" $ do
      it "allows for a bold string on its own" $ do
        "__bold string__" `shouldParseTo`
          DocBold "bold string"

      it "bolds inline correctly" $ do
        "hello __everyone__ there" `shouldParseTo`
          "hello "
           <> DocBold "everyone" <> " there"

      it "bolds unicode" $ do
        "__灼眼のシャナ__" `shouldParseTo`
          DocBold "灼眼のシャナ"

      it "does not do __multi-line\\n bold__" $ do
        " __multi-line\n bold__" `shouldParseTo` "__multi-line\n bold__"

      it "allows other markup inside of bold" $ do
        "__/inner emphasis/__" `shouldParseTo`
          (DocBold $ DocEmphasis "inner emphasis")

      it "doesn't mangle inner markup unicode" $ do
        "__/灼眼のシャナ &#65;/__" `shouldParseTo`
          (DocBold $ DocEmphasis "灼眼のシャナ A")

      it "properly converts HTML escape sequences" $ do
        "__&#65;&#65;&#65;&#65;__" `shouldParseTo`
          DocBold "AAAA"

      it "allows to escape the bold delimiter inside of bold" $ do
        "__bo\\__ld__" `shouldParseTo`
          DocBold "bo__ld"

    context "when parsing monospaced text" $ do
      it "parses simple monospaced text" $ do
        "@foo@" `shouldParseTo` DocMonospaced "foo"

      it "parses inline monospaced text" $ do
        "foo @bar@ baz" `shouldParseTo` "foo " <> DocMonospaced "bar" <> " baz"

      it "allows to escape @" $ do
        "@foo \\@ bar@" `shouldParseTo` DocMonospaced "foo @ bar"

      it "accepts unicode" $ do
        "@foo 灼眼のシャナ bar@" `shouldParseTo` DocMonospaced "foo 灼眼のシャナ bar"

      it "accepts other markup in monospaced text" $ do
        "@/foo/@" `shouldParseTo` DocMonospaced (DocEmphasis "foo")

      it "requires the closing @" $ do
        "@foo /bar/ baz" `shouldParseTo` "@foo " <> DocEmphasis "bar" <> " baz"

    context "when parsing module names" $ do
      it "accepts a simple module name" $ do
        "\"Foo\"" `shouldParseTo` DocModule "Foo"

      it "accepts a module name with dots" $ do
        "\"Foo.Bar.Baz\"" `shouldParseTo` DocModule "Foo.Bar.Baz"

      it "accepts a module name with unicode" $ do
        "\"Foo.Barλ\"" `shouldParseTo` DocModule "Foo.Barλ"

      it "parses a module inline" $ do
        "This is a \"Module\"." `shouldParseTo` ("This is a " <> (DocModule "Module" <> "."))

      it "rejects empty module name" $ do
        "\"\"" `shouldParseTo` "\"\""

      it "rejects a module name with a trailing dot" $ do
        "\"Foo.\"" `shouldParseTo` "\"Foo.\""

      it "rejects a module name with a space" $ do
        "\"Foo Bar\"" `shouldParseTo` "\"Foo Bar\""

      it "rejects a module name with invalid characters" $ do
        "\"Foo&[{}(=*)+]!\"" `shouldParseTo` "\"Foo&[{}(=*)+]!\""

  describe "parseParas" $ do
    let infix 1 `shouldParseTo`
        shouldParseTo :: String -> Doc RdrName -> Expectation
        shouldParseTo input ast = parseParas input `shouldBe` ast

    it "is total" $ do
      property $ \xs ->
        (length . show . parseParas) xs `shouldSatisfy` (> 0)

    context "when parsing text paragraphs" $ do
      let filterSpecial = filter (`notElem` (".(=#-[*`\v\f\n\t\r\\\"'_/@<> " :: String))

      it "parses an empty paragraph" $ do
        "" `shouldParseTo` DocEmpty

      it "parses a simple text paragraph" $ do
        "foo bar baz" `shouldParseTo` DocParagraph "foo bar baz"

      it "accepts markup in text paragraphs" $ do
        "foo /bar/ baz" `shouldParseTo` DocParagraph ("foo " <> DocEmphasis "bar" <> " baz")

      it "preserve all regular characters" $ do
        property $ \xs -> let input = filterSpecial xs in (not . null) input ==>
          input `shouldParseTo` DocParagraph (DocString input)

      it "separates paragraphs by empty lines" $ do
        unlines [
            "foo"
          , " \t "
          , "bar"
          ] `shouldParseTo` DocParagraph "foo" <> DocParagraph "bar"

      context "when a pragraph only contains monospaced text" $ do
        it "turns it into a code block" $ do
          "@foo@" `shouldParseTo` DocCodeBlock "foo"

    context "when parsing birdtracks" $ do
      it "parses them as a code block" $ do
        unlines [
            ">foo"
          , ">bar"
          , ">baz"
          ] `shouldParseTo` DocCodeBlock "foo\nbar\nbaz"

      it "ignores leading whitespace" $ do
        unlines [
            " >foo"
          , " \t >bar"
          , " >baz"
          ]
        `shouldParseTo` DocCodeBlock "foo\nbar\nbaz"

      it "ignores nested markup" $ do
        unlines [
            ">/foo/"
          ] `shouldParseTo` DocCodeBlock "/foo/"

      it "treats them as regular text inside text paragraphs" $ do
        unlines [
            "foo"
          , ">bar"
          ] `shouldParseTo` DocParagraph "foo\n>bar"

    context "when parsing code blocks" $ do
      it "accepts a simple code block" $ do
        unlines [
            "@"
          , "foo"
          , "bar"
          , "baz"
          , "@"
          ] `shouldParseTo` DocCodeBlock "foo\nbar\nbaz\n"

      it "ignores trailing whitespace after the opening @" $ do
        unlines [
            "@   "
          , "foo"
          , "@"
          ] `shouldParseTo` DocCodeBlock "foo\n"

      it "rejects code blocks that are not closed" $ do
        unlines [
            "@"
          , "foo"
          ] `shouldParseTo` DocParagraph "@\nfoo"

      it "accepts nested markup" $ do
        unlines [
            "@"
          , "/foo/"
          , "@"
          ] `shouldParseTo` DocCodeBlock (DocEmphasis "foo" <> "\n")

      it "allows to escape the @" $ do
        unlines [
            "@"
          , "foo"
          , "\\@"
          , "bar"
          , "@"
          ] `shouldParseTo` DocCodeBlock "foo\n@\nbar\n"

    context "when parsing examples" $ do
      it "parses a simple example" $ do
        ">>> foo" `shouldParseTo` DocExamples [Example "foo" []]

      it "parses an example with result" $ do
        unlines [
            ">>> foo"
          , "bar"
          , "baz"
          ] `shouldParseTo` DocExamples [Example "foo" ["bar", "baz"]]

      it "parses consecutive examples" $ do
        unlines [
            ">>> fib 5"
          , "5"
          , ">>> fib 10"
          , "55"
          ] `shouldParseTo` DocExamples [
            Example "fib 5" ["5"]
          , Example "fib 10" ["55"]
          ]

      it "requires an example to be separated from a previous paragraph by an empty line" $ do
        unlines [
            "foobar"
          , ""
          , ">>> fib 10"
          , "55"
          ] `shouldParseTo` DocParagraph "foobar"
                         <> DocExamples [Example "fib 10" ["55"]]

      it "parses bird-tracks inside of paragraphs as plain strings" $ do
        let xs = "foo\n>>> bar"
        xs `shouldParseTo` DocParagraph (DocString xs)

      it "skips empty lines in front of an example" $ do
        "\n   \n\n>>> foo" `shouldParseTo` DocExamples [Example "foo" []]

      it "terminates example on empty line" $ do
        unlines [
            ">>> foo"
          , "bar"
          , "    "
          , "baz"
          ]
        `shouldParseTo`
          DocExamples [Example "foo" ["bar"]] <> DocParagraph "baz"

      it "parses a <BLANKLINE> result as an empty result" $ do
        unlines [
            ">>> foo"
          , "bar"
          , "<BLANKLINE>"
          , "baz"
          ]
        `shouldParseTo` DocExamples [Example "foo" ["bar", "", "baz"]]

      it "accepts unicode in examples" $ do
        ">>> 灼眼\nシャナ" `shouldParseTo` DocExamples [Example "灼眼" ["シャナ"]]

      context "when prompt is prefixed by whitespace" $ do
        it "strips the exact same amount of whitespace from result lines" $ do
          unlines [
              "   >>> foo"
            , "   bar"
            , "   baz"
            ] `shouldParseTo` DocExamples [Example "foo" ["bar", "baz"]]

        it "preserves additional whitespace" $ do
          unlines [
              "   >>> foo"
            , "    bar"
            ] `shouldParseTo` DocExamples [Example "foo" [" bar"]]

        it "keeps original if stripping is not possible" $ do
          unlines [
              "   >>> foo"
            , " bar"
            ] `shouldParseTo` DocExamples [Example "foo" [" bar"]]

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
        "  prop>     xs == (reverse $ reverse xs)  " `shouldParseTo`
          DocProperty "xs == (reverse $ reverse xs)"

    context "when parsing unordered lists" $ do
      it "parses a simple list" $ do
        unlines [
            " * one"
          , " * two"
          , " * three"
          ]
        `shouldParseTo` DocUnorderedList [
            DocParagraph "one\n"
          , DocParagraph "two\n"
          , DocParagraph "three\n"
          ]

      it "ignores empty lines between list items" $ do
        unlines [
            "* one"
          , ""
          , "* two"
          ]
        `shouldParseTo` DocUnorderedList [
            DocParagraph "one\n"
          , DocParagraph "two\n"
          ]

      it "accepts an empty list item" $ do
        "*" `shouldParseTo` DocUnorderedList [DocParagraph DocEmpty]

      it "accepts multi-line list items" $ do
        unlines [
            "* point one"
          , "  more one"
          , "* point two"
          , "more two"
          ]
        `shouldParseTo` DocUnorderedList [
            DocParagraph "point one\n  more one\n"
          , DocParagraph "point two\nmore two\n"
          ]

      it "accepts markup in list items" $ do
        "* /foo/" `shouldParseTo` DocUnorderedList [DocParagraph (DocEmphasis "foo" <> "\n")]

      it "requires empty lines between list and other paragraphs" $ do
        unlines [
            "foo"
          , ""
          , "* bar"
          , ""
          , "baz"
          ]
        `shouldParseTo` DocParagraph "foo" <> DocUnorderedList [DocParagraph "bar\n"] <> DocParagraph "baz"

    context "when parsing ordered lists" $ do
      it "parses a simple list" $ do
        unlines [
            " 1. one"
          , " (1) two"
          , " 3. three"
          ]
        `shouldParseTo` DocOrderedList [
            DocParagraph "one\n"
          , DocParagraph "two\n"
          , DocParagraph "three\n"
          ]

      it "ignores empty lines between list items" $ do
        unlines [
            "1. one"
          , ""
          , "2. two"
          ]
        `shouldParseTo` DocOrderedList [
            DocParagraph "one\n"
          , DocParagraph "two\n"
          ]

      it "accepts an empty list item" $ do
        "1." `shouldParseTo` DocOrderedList [DocParagraph DocEmpty]

      it "accepts multi-line list items" $ do
        unlines [
            "1. point one"
          , "  more one"
          , "1. point two"
          , "more two"
          ]
        `shouldParseTo` DocOrderedList [
            DocParagraph "point one\n  more one\n"
          , DocParagraph "point two\nmore two\n"
          ]

      it "accepts markup in list items" $ do
        "1. /foo/" `shouldParseTo` DocOrderedList [DocParagraph (DocEmphasis "foo" <> "\n")]

      it "requires empty lines between list and other paragraphs" $ do
        unlines [
            "foo"
          , ""
          , "1. bar"
          , ""
          , "baz"
          ]
        `shouldParseTo` DocParagraph "foo" <> DocOrderedList [DocParagraph "bar\n"] <> DocParagraph "baz"

    context "when parsing definition lists" $ do
      it "parses a simple list" $ do
        unlines [
            " [foo] one"
          , " [bar] two"
          , " [baz] three"
          ]
        `shouldParseTo` DocDefList [
            ("foo", "one\n")
          , ("bar", "two\n")
          , ("baz", "three\n")
          ]

      it "ignores empty lines between list items" $ do
        unlines [
            "[foo] one"
          , ""
          , "[bar] two"
          ]
        `shouldParseTo` DocDefList [
            ("foo", "one\n")
          , ("bar", "two\n")
          ]

      it "accepts an empty list item" $ do
        "[foo]" `shouldParseTo` DocDefList [("foo", DocEmpty)]

      it "accepts multi-line list items" $ do
        unlines [
            "[foo] point one"
          , "  more one"
          , "[bar] point two"
          , "more two"
          ]
        `shouldParseTo` DocDefList [
            ("foo", "point one\n  more one\n")
          , ("bar", "point two\nmore two\n")
          ]

      it "accepts markup in list items" $ do
        "[foo] /foo/" `shouldParseTo` DocDefList [("foo", DocEmphasis "foo" <> "\n")]

      it "accepts markup for the label" $ do
        "[/foo/] bar" `shouldParseTo` DocDefList [(DocEmphasis "foo", "bar\n")]

      it "requires empty lines between list and other paragraphs" $ do
        unlines [
            "foo"
          , ""
          , "[foo] bar"
          , ""
          , "baz"
          ]
        `shouldParseTo` DocParagraph "foo" <> DocDefList [("foo", "bar\n")] <> DocParagraph "baz"

    context "when parsing consecutive paragraphs" $ do
      it "accepts consecutive lists" $ do
        unlines [
            "   * foo"
          , ""
          , "   - bar"
          , ""
          , "   (1) ordered foo"
          , " "
          , "   2. ordered bar"
          , "   "
          , "   [cat] kitten"
          , "   "
          , "   [pineapple] fruit"
          ] `shouldParseTo` DocUnorderedList [
            DocParagraph "foo\n"
          , DocParagraph "bar\n"
          ] <> DocOrderedList [
            DocParagraph "ordered foo\n"
          , DocParagraph "ordered bar\n"
          ] <> DocDefList [
            ("cat", "kitten\n")
          , ("pineapple", "fruit\n")
          ]

    context "when parsing function documentation headers" $ do
      it "can parse a simple header" $ do
        "= Header 1\nHello." `shouldParseTo`
          DocParagraph (DocHeader (Header 1 "Header 1"))
          <> DocParagraph "Hello."

      it "allow consecutive headers" $ do
        "= Header 1\n== Header 2" `shouldParseTo`
          DocParagraph (DocHeader (Header 1 "Header 1"))
          <> DocParagraph (DocHeader (Header 2 "Header 2"))

      it "accepts markup in the header" $ do
        "= /Header/ __1__\nFoo" `shouldParseTo`
          DocParagraph (DocHeader
                        (Header 1 (DocEmphasis "Header" <> " " <> DocBold "1")))
          <> DocParagraph "Foo"
