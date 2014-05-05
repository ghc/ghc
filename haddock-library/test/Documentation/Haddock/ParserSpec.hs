{-# LANGUAGE OverloadedStrings, StandaloneDeriving
             , FlexibleInstances, UndecidableInstances
             , IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Documentation.Haddock.ParserSpec (main, spec) where

import           Data.Monoid
import           Data.String
import qualified Documentation.Haddock.Parser as Parse
import           Documentation.Haddock.Types
import           Test.Hspec
import           Test.QuickCheck

type Doc id = DocH () id

deriving instance Show a => Show (Header a)
deriving instance Show a => Show (Doc a)
deriving instance Eq a => Eq (Header a)
deriving instance Eq a => Eq (Doc a)

instance IsString (Doc String) where
  fromString = DocString

instance IsString a => IsString (Maybe a) where
  fromString = Just . fromString

parseParas :: String -> Doc String
parseParas = Parse.toRegular . Parse.parseParas

parseString :: String -> Doc String
parseString = Parse.toRegular . Parse.parseString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseString" $ do
    let infix 1 `shouldParseTo`
        shouldParseTo :: String -> Doc String -> Expectation
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

      context "when parsing strings contaning numeric character references" $ do
        it "will implicitly convert digits to characters" $ do
          "&#65;&#65;&#65;&#65;" `shouldParseTo` "AAAA"

          "&#28796;&#30524;&#12398;&#12471;&#12515;&#12490;"
            `shouldParseTo` "灼眼のシャナ"

        it "will implicitly convert hex encoded characters" $ do
          "&#x65;&#x65;&#x65;&#x65;" `shouldParseTo` "eeee"

    context "when parsing identifiers" $ do
      it "parses identifiers enclosed within single ticks" $ do
        "'foo'" `shouldParseTo` DocIdentifier "foo"

      it "parses identifiers enclosed within backticks" $ do
        "`foo`" `shouldParseTo` DocIdentifier "foo"

      it "parses a word with an one of the delimiters in it as DocString" $ do
          "don't" `shouldParseTo` "don't"

      it "doesn't pass pairs of delimiters with spaces between them" $ do
        "hel'lo w'orld" `shouldParseTo` "hel'lo w'orld"

      it "don't use apostrophe's in the wrong place's" $ do
        " don't use apostrophe's in the wrong place's" `shouldParseTo`
          "don't use apostrophe's in the wrong place's"

    context "when parsing URLs" $ do
      let hyperlink :: String -> Maybe String -> Doc String
          hyperlink url = DocHyperlink . Hyperlink url

      it "parses a URL" $ do
        "<http://example.com/>" `shouldParseTo` hyperlink "http://example.com/" Nothing

      it "accepts an optional label" $ do
        "<http://example.com/ some link>" `shouldParseTo` hyperlink "http://example.com/" "some link"

      it "does not accept newlines in label" $ do
        "<foo bar\nbaz>" `shouldParseTo` "<foo bar\nbaz>"

      -- new behaviour test, this will be now consistent with other markup
      it "allows us to escape > inside the URL" $ do
        "<http://examp\\>le.com>" `shouldParseTo`
          hyperlink "http://examp>le.com" Nothing

        "<http://exa\\>mp\\>le.com>" `shouldParseTo`
          hyperlink "http://exa>mp>le.com" Nothing

        -- Likewise in label
        "<http://example.com f\\>oo>" `shouldParseTo`
          hyperlink "http://example.com" "f>oo"

      it "parses inline URLs" $ do
        "foo <http://example.com/> bar" `shouldParseTo`
          "foo " <> hyperlink "http://example.com/" Nothing <> " bar"

      it "doesn't allow for multi-line link tags" $ do
        "<ba\nz aar>" `shouldParseTo` "<ba\nz aar>"

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
      let picture :: String -> Maybe String -> Doc String
          picture uri = DocPic . Picture uri

      it "parses a simple picture" $ do
        "<<baz>>" `shouldParseTo` picture "baz" Nothing

      it "parses a picture with a title" $ do
        "<<b a z>>" `shouldParseTo` picture "b" (Just "a z")

      it "parses a picture with unicode" $ do
        "<<灼眼のシャナ>>" `shouldParseTo` picture "灼眼のシャナ" Nothing

      it "allows for escaping of the closing tags" $ do
        "<<ba\\>>z>>" `shouldParseTo` picture "ba>>z" Nothing

      it "doesn't allow for multi-line picture tags" $ do
        "<<ba\nz aar>>" `shouldParseTo` "<<ba\nz aar>>"

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

      it "doesn't allow for empty bold" $ do
        "____" `shouldParseTo` "____"

    context "when parsing module strings" $ do
      it "should parse a module on its own" $ do
        "\"Module\"" `shouldParseTo`
          DocModule "Module"

      it "should parse a module inline" $ do
        "This is a \"Module\"." `shouldParseTo`
          "This is a " <> DocModule "Module" <> "."

      it "can accept a simple module name" $ do
        "\"Hello\"" `shouldParseTo` DocModule "Hello"

      it "can accept a module name with dots" $ do
        "\"Hello.World\"" `shouldParseTo` DocModule "Hello.World"

      it "can accept a module name with unicode" $ do
        "\"Hello.Worldλ\"" `shouldParseTo` DocModule "Hello.Worldλ"

      it "parses a module name with a trailing dot as regular quoted string" $ do
        "\"Hello.\"" `shouldParseTo` "\"Hello.\""

      it "parses a module name with a space as regular quoted string" $ do
        "\"Hello World\"" `shouldParseTo` "\"Hello World\""

      it "parses a module name with invalid characters as regular quoted string" $ do
        "\"Hello&[{}(=*)+]!\"" `shouldParseTo` "\"Hello&[{}(=*)+]!\""

      it "accepts a module name with unicode" $ do
        "\"Foo.Barλ\"" `shouldParseTo` DocModule "Foo.Barλ"

      it "treats empty module name as regular double quotes" $ do
        "\"\"" `shouldParseTo` "\"\""

  describe "parseParas" $ do
    let infix 1 `shouldParseTo`
        shouldParseTo :: String -> Doc String -> Expectation
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

      it "strips one leading space from each line of the block" $ do
        unlines [
            "> foo"
          , ">  bar"
          , "> baz"
          ] `shouldParseTo` DocCodeBlock "foo\n bar\nbaz"

      it "ignores empty lines when stripping spaces" $ do
        unlines [
            "> foo"
          , ">"
          , "> bar"
          ] `shouldParseTo` DocCodeBlock "foo\n\nbar"

      context "when any non-empty line does not start with a space" $ do
        it "does not strip any spaces" $ do
          unlines [
              ">foo"
            , ">  bar"
            ] `shouldParseTo` DocCodeBlock "foo\n  bar"

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

      it "accepts horizontal space before the @" $ do
        unlines [ "     @"
                , "foo"
                , ""
                , "bar"
                , "@"
                ] `shouldParseTo` DocCodeBlock "foo\n\nbar\n"

      it "strips a leading space from a @ block if present" $ do
        unlines [ " @"
                , " hello"
                , " world"
                , " @"
                ] `shouldParseTo` DocCodeBlock "hello\nworld\n"

        unlines [ " @"
                , " hello"
                , ""
                , " world"
                , " @"
                ] `shouldParseTo` DocCodeBlock "hello\n\nworld\n"

      it "only drops whitespace if there's some before closing @" $ do
        unlines [ "@"
                , "    Formatting"
                , "        matters."
                , "@"
                ]
          `shouldParseTo` DocCodeBlock "    Formatting\n        matters.\n"

      it "accepts unicode" $ do
        "@foo 灼眼のシャナ bar@" `shouldParseTo` DocCodeBlock "foo 灼眼のシャナ bar"

      it "requires the closing @" $ do
        "@foo /bar/ baz"
          `shouldParseTo` DocParagraph ("@foo " <> DocEmphasis "bar" <> " baz")


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

      it ("requires an example to be separated"
          ++ " from a previous paragraph by an empty line") $ do
        "foobar\n\n>>> fib 10\n55" `shouldParseTo`
          DocParagraph "foobar"
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


    context "when parsing paragraphs nested in lists" $ do
      it "can nest the same type of list" $ do
        "* foo\n\n    * bar" `shouldParseTo`
          DocUnorderedList [ DocParagraph $ "foo"
                             <> DocUnorderedList [DocParagraph "bar"]]

      it "can nest another type of list inside" $ do
        "* foo\n\n    1. bar" `shouldParseTo`
          DocUnorderedList [ DocParagraph $ "foo"
                             <> DocOrderedList [DocParagraph "bar"]]

      it "can nest a code block inside" $ do
        "* foo\n\n    @foo bar baz@" `shouldParseTo`
          DocUnorderedList [ DocParagraph $ "foo"
                             <> DocCodeBlock "foo bar baz"]

        "* foo\n\n    @\n    foo bar baz\n    @" `shouldParseTo`
          DocUnorderedList [ DocParagraph $ "foo"
                             <> DocCodeBlock "foo bar baz\n"]

      it "can nest more than one level" $ do
        "* foo\n\n    * bar\n\n        * baz\n        qux" `shouldParseTo`
          DocUnorderedList [ DocParagraph $ "foo"
                             <> DocUnorderedList [ DocParagraph $ "bar"
                                                   <> DocUnorderedList [DocParagraph "baz\nqux"]
                                                 ]
                           ]

      it "won't fail on not fully indented paragraph" $ do
        "* foo\n\n    * bar\n\n        * qux\nquux" `shouldParseTo`
          DocUnorderedList [ DocParagraph $ "foo"
                             <> DocUnorderedList [ DocParagraph "bar" ]
                           , DocParagraph "qux\nquux"]


      it "can nest definition lists" $ do
        "[a] foo\n\n    [b] bar\n\n        [c] baz\n        qux" `shouldParseTo`
          DocDefList [ ("a", "foo"
                             <> DocDefList [ ("b", "bar"
                                                   <> DocDefList [("c", "baz\nqux")])
                                           ])
                     ]

      it "can come back to top level with a different list" $ do
        "* foo\n\n    * bar\n\n1. baz" `shouldParseTo`
          DocUnorderedList [ DocParagraph $ "foo"
                             <> DocUnorderedList [ DocParagraph "bar" ]
                           ]
          <> DocOrderedList [ DocParagraph "baz" ]

      it "definition lists can come back to top level with a different list" $ do
        "[foo] foov\n\n    [bar] barv\n\n1. baz" `shouldParseTo`
          DocDefList [ ("foo", "foov"
                               <> DocDefList [ ("bar", "barv") ])
                     ]
          <> DocOrderedList [ DocParagraph "baz" ]

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
            DocParagraph "one"
          , DocParagraph "two"
          , DocParagraph "three"
          ]

      it "ignores empty lines between list items" $ do
        unlines [
            "* one"
          , ""
          , "* two"
          ]
        `shouldParseTo` DocUnorderedList [
            DocParagraph "one"
          , DocParagraph "two"
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
            DocParagraph "point one\n  more one"
          , DocParagraph "point two\nmore two"
          ]

      it "accepts markup in list items" $ do
        "* /foo/" `shouldParseTo` DocUnorderedList [DocParagraph (DocEmphasis "foo")]

      it "requires empty lines between list and other paragraphs" $ do
        unlines [
            "foo"
          , ""
          , "* bar"
          , ""
          , "baz"
          ]
        `shouldParseTo` DocParagraph "foo" <> DocUnorderedList [DocParagraph "bar"] <> DocParagraph "baz"

    context "when parsing ordered lists" $ do
      it "parses a simple list" $ do
        unlines [
            " 1. one"
          , " (1) two"
          , " 3. three"
          ]
        `shouldParseTo` DocOrderedList [
            DocParagraph "one"
          , DocParagraph "two"
          , DocParagraph "three"
          ]

      it "ignores empty lines between list items" $ do
        unlines [
            "1. one"
          , ""
          , "2. two"
          ]
        `shouldParseTo` DocOrderedList [
            DocParagraph "one"
          , DocParagraph "two"
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
            DocParagraph "point one\n  more one"
          , DocParagraph "point two\nmore two"
          ]

      it "accepts markup in list items" $ do
        "1. /foo/" `shouldParseTo` DocOrderedList [DocParagraph (DocEmphasis "foo")]

      it "requires empty lines between list and other paragraphs" $ do
        unlines [
            "foo"
          , ""
          , "1. bar"
          , ""
          , "baz"
          ]
        `shouldParseTo` DocParagraph "foo" <> DocOrderedList [DocParagraph "bar"] <> DocParagraph "baz"

    context "when parsing definition lists" $ do
      it "parses a simple list" $ do
        unlines [
            " [foo] one"
          , " [bar] two"
          , " [baz] three"
          ]
        `shouldParseTo` DocDefList [
            ("foo", "one")
          , ("bar", "two")
          , ("baz", "three")
          ]

      it "ignores empty lines between list items" $ do
        unlines [
            "[foo] one"
          , ""
          , "[bar] two"
          ]
        `shouldParseTo` DocDefList [
            ("foo", "one")
          , ("bar", "two")
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
            ("foo", "point one\n  more one")
          , ("bar", "point two\nmore two")
          ]

      it "accepts markup in list items" $ do
        "[foo] /foo/" `shouldParseTo` DocDefList [("foo", DocEmphasis "foo")]

      it "accepts markup for the label" $ do
        "[/foo/] bar" `shouldParseTo` DocDefList [(DocEmphasis "foo", "bar")]

      it "requires empty lines between list and other paragraphs" $ do
        unlines [
            "foo"
          , ""
          , "[foo] bar"
          , ""
          , "baz"
          ]
        `shouldParseTo` DocParagraph "foo" <> DocDefList [("foo", "bar")] <> DocParagraph "baz"

    context "when parsing consecutive paragraphs" $ do
      it "will not capture irrelevant consecutive lists" $ do
        unlines [ "   * bullet"
                , ""
                , ""
                , "   - different bullet"
                , ""
                , ""
                , "   (1) ordered"
                , " "
                , "   2. different bullet"
                , "   "
                , "   [cat] kitten"
                , "   "
                , "   [pineapple] fruit"
                ] `shouldParseTo`
          DocUnorderedList [ DocParagraph "bullet"
                           , DocParagraph "different bullet"]
          <> DocOrderedList [ DocParagraph "ordered"
                            , DocParagraph "different bullet"
                            ]
          <> DocDefList [ ("cat", "kitten")
                        , ("pineapple", "fruit")
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
