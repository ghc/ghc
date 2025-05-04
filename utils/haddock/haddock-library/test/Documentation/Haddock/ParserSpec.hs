{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Documentation.Haddock.ParserSpec (main, spec) where

import Data.Char (isSpace)
import Data.String
import Test.Hspec
import Test.QuickCheck
import Prelude hiding ((<>))

import Documentation.Haddock.Doc (docAppend)
import qualified Documentation.Haddock.Parser as Parse
import Documentation.Haddock.Types

infixr 6 <>
(<>) :: Doc id -> Doc id -> Doc id
(<>) = docAppend

type Doc id = DocH () id

instance IsString (Doc String) where
  fromString = DocString

instance IsString a => IsString (Maybe a) where
  fromString = Just . fromString

parseParas :: String -> MetaDoc () String
parseParas = overDoc Parse.toRegular . Parse.parseParas Nothing

parseString :: String -> Doc String
parseString = Parse.toRegular . Parse.parseString

hyperlink :: String -> Maybe (Doc String) -> Doc String
hyperlink url = DocHyperlink . Hyperlink url

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

      it "allows to backslash-escape characters except \\r" $ do
        property $ \y -> case y of
          '\r' -> "\\\r" `shouldParseTo` DocString "\\"
          x -> ['\\', x] `shouldParseTo` DocString [x]

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

      it "parses identifiers preceded by a backtick and followed by a single quote" $ do
        "`foo'" `shouldParseTo` DocIdentifier "foo"

      it "parses identifiers preceded by a single quote and followed by a backtick" $ do
        "'foo`" `shouldParseTo` DocIdentifier "foo"

      it "can parse a constructor identifier" $ do
        "'Foo'" `shouldParseTo` DocIdentifier "Foo"

      it "can parse a qualified identifier" $ do
        "'Foo.bar'" `shouldParseTo` DocIdentifier "Foo.bar"

      it "parses a word with an one of the delimiters in it as DocString" $ do
        "don't" `shouldParseTo` "don't"

      it "doesn't pass pairs of delimiters with spaces between them" $ do
        "hel'lo w'orld" `shouldParseTo` "hel'lo w'orld"

      it "don't use apostrophe's in the wrong place's" $ do
        " don't use apostrophe's in the wrong place's"
          `shouldParseTo` "don't use apostrophe's in the wrong place's"

      it "doesn't parse empty identifiers" $ do
        "``" `shouldParseTo` "``"

      it "can parse an identifier in infix notation enclosed within backticks" $ do
        "``infix``" `shouldParseTo` DocIdentifier "`infix`"

      it "can parse identifiers containing a single quote" $ do
        "'don't'" `shouldParseTo` DocIdentifier "don't"

      it "can parse identifiers ending with a single quote" $ do
        "'foo''" `shouldParseTo` DocIdentifier "foo'"

      it "can parse identifiers in backticks ending with a single quote" $ do
        "`foo'`" `shouldParseTo` DocIdentifier "foo'"

      it "can parse an identifier containing a digit" $ do
        "'f0'" `shouldParseTo` DocIdentifier "f0"

      it "can parse an identifier containing unicode characters" $ do
        "'λ'" `shouldParseTo` DocIdentifier "λ"

      it "can parse a single quote followed by an identifier" $ do
        "''foo'" `shouldParseTo` "'" <> DocIdentifier "foo"

      it "can parse an identifier that starts with an underscore" $ do
        "'_x'" `shouldParseTo` DocIdentifier "_x"

      it "can parse value-namespaced identifiers" $ do
        "v'foo'" `shouldParseTo` DocIdentifier "foo"

      it "can parse type-namespaced identifiers" $ do
        "t'foo'" `shouldParseTo` DocIdentifier "foo"

      it "can parse parenthesized operators and backticked identifiers" $ do
        "'(<|>)'" `shouldParseTo` DocIdentifier "(<|>)"
        "'`elem`'" `shouldParseTo` DocIdentifier "`elem`"

      it "can properly figure out the end of identifiers" $ do
        "'DbModule'/'DbUnitId'" `shouldParseTo` DocIdentifier "DbModule" <> "/" <> DocIdentifier "DbUnitId"

    context "when parsing operators" $ do
      it "can parse an operator enclosed within single quotes" $ do
        "'.='" `shouldParseTo` DocIdentifier ".="

      it "can parse a qualified operator" $ do
        "'F..'" `shouldParseTo` DocIdentifier "F.."

      it "can parse a constructor operator" $ do
        "':='" `shouldParseTo` DocIdentifier ":="

      it "can parse a qualified constructor operator" $ do
        "'F.:='" `shouldParseTo` DocIdentifier "F.:="

      it "can parse a unicode operator" $ do
        "'∧'" `shouldParseTo` DocIdentifier "∧"

    context "when parsing URLs" $ do
      it "parses a URL" $ do
        "<http://example.com/>" `shouldParseTo` hyperlink "http://example.com/" Nothing

      it "accepts an optional label" $ do
        "<http://example.com/ some link>" `shouldParseTo` hyperlink "http://example.com/" "some link"

      it "does not accept newlines in label" $ do
        "<foo bar\nbaz>" `shouldParseTo` "<foo bar\nbaz>"

      -- new behaviour test, this will be now consistent with other markup
      it "allows us to escape > inside the URL" $ do
        "<http://examp\\>le.com>"
          `shouldParseTo` hyperlink "http://examp>le.com" Nothing

        "<http://exa\\>mp\\>le.com>"
          `shouldParseTo` hyperlink "http://exa>mp>le.com" Nothing

        -- Likewise in label
        "<http://example.com f\\>oo>"
          `shouldParseTo` hyperlink "http://example.com" "f>oo"

      it "parses inline URLs" $ do
        "foo <http://example.com/> bar"
          `shouldParseTo` "foo "
          <> hyperlink "http://example.com/" Nothing
          <> " bar"

      it "doesn't allow for multi-line link tags" $ do
        "<ba\nz aar>" `shouldParseTo` "<ba\nz aar>"

      context "when parsing markdown links" $ do
        it "parses a simple link" $ do
          "[some label](url)"
            `shouldParseTo` hyperlink "url" "some label"

        it "allows whitespace between label and URL" $ do
          "[some label] \t (url)"
            `shouldParseTo` hyperlink "url" "some label"

        it "allows newlines in label" $ do
          "[some\n\nlabel](url)"
            `shouldParseTo` hyperlink "url" "some\n\nlabel"

        it "allows escaping in label" $ do
          "[some\\] label](url)"
            `shouldParseTo` hyperlink "url" "some] label"

        it "strips leading and trailing whitespace from label" $ do
          "[  some label  ](url)"
            `shouldParseTo` hyperlink "url" "some label"

        it "rejects whitespace in URL" $ do
          "[some label]( url)"
            `shouldParseTo` "[some label]( url)"

        it "allows inline markup in the label" $ do
          "[something /emphasized/](url)"
            `shouldParseTo` hyperlink "url" (Just ("something " <> DocEmphasis "emphasized"))

        context "when URL is on a separate line" $ do
          it "allows URL to be on a separate line" $ do
            "[some label]\n(url)"
              `shouldParseTo` hyperlink "url" "some label"

          it "allows leading whitespace" $ do
            "[some label]\n  \t (url)"
              `shouldParseTo` hyperlink "url" "some label"

          it "rejects additional newlines" $ do
            "[some label]\n\n(url)"
              `shouldParseTo` "[some label]\n\n(url)"

      context "when autolinking URLs" $ do
        it "autolinks HTTP URLs" $ do
          "http://example.com/" `shouldParseTo` hyperlink "http://example.com/" Nothing

        it "autolinks HTTPS URLs" $ do
          "https://www.example.com/" `shouldParseTo` hyperlink "https://www.example.com/" Nothing

        it "autolinks FTP URLs" $ do
          "ftp://example.com/" `shouldParseTo` hyperlink "ftp://example.com/" Nothing

        it "does not include a trailing comma" $ do
          "http://example.com/, Some other sentence."
            `shouldParseTo` hyperlink "http://example.com/" Nothing
            <> ", Some other sentence."

        it "does not include a trailing dot" $ do
          "http://example.com/. Some other sentence."
            `shouldParseTo` hyperlink "http://example.com/" Nothing
            <> ". Some other sentence."

        it "does not include a trailing exclamation mark" $ do
          "http://example.com/! Some other sentence."
            `shouldParseTo` hyperlink "http://example.com/" Nothing
            <> "! Some other sentence."

        it "does not include a trailing question mark" $ do
          "http://example.com/? Some other sentence."
            `shouldParseTo` hyperlink "http://example.com/" Nothing
            <> "? Some other sentence."

        it "autolinks URLs occuring mid-sentence with multiple ‘/’s" $ do
          "foo https://example.com/example bar"
            `shouldParseTo` "foo "
            <> hyperlink "https://example.com/example" Nothing
            <> " bar"

    context "when parsing images" $ do
      let image :: String -> Maybe String -> Doc String
          image uri = DocPic . Picture uri

      it "accepts markdown syntax for images" $ do
        "![label](url)" `shouldParseTo` image "url" "label"

      it "accepts Unicode" $ do
        "![灼眼のシャナ](url)" `shouldParseTo` image "url" "灼眼のシャナ"

      it "supports deprecated picture syntax" $ do
        "<<baz>>" `shouldParseTo` image "baz" Nothing

      it "supports title for deprecated picture syntax" $ do
        "<<b a z>>" `shouldParseTo` image "b" "a z"

    context "when parsing display math" $ do
      it "accepts markdown syntax for display math containing newlines" $ do
        "\\[\\pi\n\\pi\\]" `shouldParseTo` DocMathDisplay "\\pi\n\\pi"

    context "when parsing anchors" $ do
      it "parses a single word anchor" $ do
        "#foo#" `shouldParseTo` DocAName "foo"

      -- Spaces are not allowed:
      -- https://www.w3.org/TR/html51/dom.html#the-id-attribute
      it "doesn't parse a multi word anchor" $ do
        "#foo bar#" `shouldParseTo` "#foo bar#"

      it "parses a unicode anchor" $ do
        "#灼眼のシャナ#" `shouldParseTo` DocAName "灼眼のシャナ"

      it "does not accept newlines in anchors" $ do
        "#foo\nbar#" `shouldParseTo` "#foo\nbar#"

      it "accepts anchors mid-paragraph" $ do
        "Hello #someAnchor# world!"
          `shouldParseTo` "Hello "
          <> DocAName "someAnchor"
          <> " world!"

      it "does not accept empty anchors" $ do
        "##" `shouldParseTo` "##"

      it "does not accept anchors containing spaces" $ do
        "{-# LANGUAGE GADTs #-}" `shouldParseTo` "{-# LANGUAGE GADTs #-}"

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
        "/foo @bar@ baz/"
          `shouldParseTo` DocEmphasis ("foo " <> DocMonospaced "bar" <> " baz")

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
        "__bold string__"
          `shouldParseTo` DocBold "bold string"

      it "bolds inline correctly" $ do
        "hello __everyone__ there"
          `shouldParseTo` "hello "
          <> DocBold "everyone"
          <> " there"

      it "bolds unicode" $ do
        "__灼眼のシャナ__"
          `shouldParseTo` DocBold "灼眼のシャナ"

      it "does not do __multi-line\\n bold__" $ do
        " __multi-line\n bold__" `shouldParseTo` "__multi-line\n bold__"

      it "allows other markup inside of bold" $ do
        "__/inner emphasis/__"
          `shouldParseTo` (DocBold $ DocEmphasis "inner emphasis")

      it "doesn't mangle inner markup unicode" $ do
        "__/灼眼のシャナ &#65;/__"
          `shouldParseTo` (DocBold $ DocEmphasis "灼眼のシャナ A")

      it "properly converts HTML escape sequences" $ do
        "__&#65;&#65;&#65;&#65;__"
          `shouldParseTo` DocBold "AAAA"

      it "allows to escape the bold delimiter inside of bold" $ do
        "__bo\\__ld__"
          `shouldParseTo` DocBold "bo__ld"

      it "doesn't allow for empty bold" $ do
        "____" `shouldParseTo` "____"

    context "when parsing module strings" $ do
      it "should parse a module on its own" $ do
        "\"Module\""
          `shouldParseTo` DocModule (ModLink "Module" Nothing)

      it "should parse a module inline" $ do
        "This is a \"Module\"."
          `shouldParseTo` "This is a "
          <> DocModule (ModLink "Module" Nothing)
          <> "."

      it "can accept a simple module name" $ do
        "\"Hello\"" `shouldParseTo` DocModule (ModLink "Hello" Nothing)

      it "can accept a module name with dots" $ do
        "\"Hello.World\"" `shouldParseTo` DocModule (ModLink "Hello.World" Nothing)

      it "can accept a module name with unicode" $ do
        "\"Hello.Worldλ\"" `shouldParseTo` DocModule (ModLink "Hello.Worldλ" Nothing)

      it "parses a module name with a trailing dot as regular quoted string" $ do
        "\"Hello.\"" `shouldParseTo` "\"Hello.\""

      it "parses a module name with a space as regular quoted string" $ do
        "\"Hello World\"" `shouldParseTo` "\"Hello World\""

      it "parses a module name with invalid characters as regular quoted string" $ do
        "\"Hello&[{}(=*)+]!\"" `shouldParseTo` "\"Hello&[{}(=*)+]!\""

      it "accepts a module name with unicode" $ do
        "\"Foo.Barλ\"" `shouldParseTo` DocModule (ModLink "Foo.Barλ" Nothing)

      it "treats empty module name as regular double quotes" $ do
        "\"\"" `shouldParseTo` "\"\""

      it "accepts anchor reference syntax as DocModule" $ do
        "\"Foo#bar\"" `shouldParseTo` DocModule (ModLink "Foo#bar" Nothing)

      it "accepts anchor with hyphen as DocModule" $ do
        "\"Foo#bar-baz\"" `shouldParseTo` DocModule (ModLink "Foo#bar-baz" Nothing)

      it "accepts old anchor reference syntax as DocModule" $ do
        "\"Foo\\#bar\"" `shouldParseTo` DocModule (ModLink "Foo\\#bar" Nothing)

    context "when parsing labeled module links" $ do
      it "parses a simple labeled module link" $ do
        "[some label](\"Some.Module\")"
          `shouldParseTo` DocModule (ModLink "Some.Module" (Just "some label"))

      it "allows escaping in label" $ do
        "[some\\] label](\"Some.Module\")"
          `shouldParseTo` DocModule (ModLink "Some.Module" (Just "some] label"))

      it "strips leading and trailing whitespace from label" $ do
        "[  some label  ](\"Some.Module\")"
          `shouldParseTo` DocModule (ModLink "Some.Module" (Just "some label"))

      it "allows whitespace in module name link" $ do
        "[some label]( \"Some.Module\"\t )"
          `shouldParseTo` DocModule (ModLink "Some.Module" (Just "some label"))

      it "allows inline markup in the label" $ do
        "[something /emphasized/](\"Some.Module\")"
          `shouldParseTo` DocModule (ModLink "Some.Module" (Just ("something " <> DocEmphasis "emphasized")))

      it "should parse a labeled module on its own" $ do
        "[label](\"Module\")"
          `shouldParseTo` DocModule (ModLink "Module" (Just "label"))

      it "should parse a labeled module inline" $ do
        "This is a [label](\"Module\")."
          `shouldParseTo` "This is a "
          <> DocModule (ModLink "Module" (Just "label"))
          <> "."

      it "can accept a labeled module name with dots" $ do
        "[label](\"Hello.World\")" `shouldParseTo` DocModule (ModLink "Hello.World" (Just "label"))

      it "can accept a labeled module name with unicode" $ do
        "[label](\"Hello.Worldλ\")" `shouldParseTo` DocModule (ModLink "Hello.Worldλ" (Just "label"))

      it "parses a labeled module name with a trailing dot as a hyperlink" $ do
        "[label](\"Hello.\")"
          `shouldParseTo` hyperlink "\"Hello.\"" (Just "label")

      it "parses a labeled module name with a space as a regular string" $ do
        "[label](\"Hello World\")" `shouldParseTo` "[label](\"Hello World\")"

      it "parses a module name with invalid characters as a hyperlink" $ do
        "[label](\"Hello&[{}(=*+]!\")"
          `shouldParseTo` hyperlink "\"Hello&[{}(=*+]!\"" (Just "label")

      it "accepts a labeled module name with unicode" $ do
        "[label](\"Foo.Barλ\")"
          `shouldParseTo` DocModule (ModLink "Foo.Barλ" (Just "label"))

      it "treats empty labeled module name as empty hyperlink" $ do
        "[label](\"\")"
          `shouldParseTo` hyperlink "\"\"" (Just "label")

      it "accepts anchor reference syntax for labeled module name" $ do
        "[label](\"Foo#bar\")"
          `shouldParseTo` DocModule (ModLink "Foo#bar" (Just "label"))

      it "accepts old anchor reference syntax for labeled module name" $ do
        "[label](\"Foo\\#bar\")"
          `shouldParseTo` DocModule (ModLink "Foo\\#bar" (Just "label"))

      it "interprets empty label as a unlabeled module name" $ do
        "[](\"Module.Name\")"
          `shouldParseTo` "[]("
          <> DocModule (ModLink "Module.Name" Nothing)
          <> ")"

  describe "parseParas" $ do
    let infix 1 `shouldParseTo`
        shouldParseTo :: String -> Doc String -> Expectation
        shouldParseTo input ast = _doc (parseParas input) `shouldBe` ast

    it "is total" $ do
      property $ \xs ->
        (length . show . parseParas) xs `shouldSatisfy` (> 0)

    -- See <https://github.com/haskell/haddock/issues/1142>
    it "doesn't crash on unicode whitespace" $ do
      "\8197" `shouldParseTo` DocEmpty

    context "when parsing @since" $ do
      it "adds specified version to the result" $ do
        parseParas "@since 0.5.0"
          `shouldBe` MetaDoc
            { _meta = Meta{_metaSince = Just (MetaSince{sincePackage = Nothing, sinceVersion = [0, 5, 0]})}
            , _doc = DocEmpty
            }

      it "ignores trailing whitespace" $ do
        parseParas "@since 0.5.0 \t "
          `shouldBe` MetaDoc
            { _meta = Meta{_metaSince = Just (MetaSince{sincePackage = Nothing, sinceVersion = [0, 5, 0]})}
            , _doc = DocEmpty
            }

      it "does not allow trailing input" $ do
        parseParas "@since 0.5.0 foo"
          `shouldBe` MetaDoc
            { _meta = Meta{_metaSince = Nothing}
            , _doc = DocParagraph "@since 0.5.0 foo"
            }

      it "parses package name" $ do
        parseParas "@since foo-bar-0.5.0"
          `shouldBe` MetaDoc
            { _meta =
                Meta
                  { _metaSince =
                      Just $
                        MetaSince
                          { sincePackage = Just "foo-bar"
                          , sinceVersion = [0, 5, 0]
                          }
                  }
            , _doc = DocEmpty
            }

      context "when given multiple times" $ do
        it "gives last occurrence precedence" $
          do
            (parseParas . unlines)
              [ "@since 0.5.0"
              , "@since 0.6.0"
              , "@since 0.7.0"
              ]
            `shouldBe` MetaDoc
              { _meta =
                  Meta
                    { _metaSince =
                        Just $
                          MetaSince
                            { sincePackage = Nothing
                            , sinceVersion = [0, 7, 0]
                            }
                    }
              , _doc = DocEmpty
              }

    context "when parsing text paragraphs" $ do
      let isSpecial c = isSpace c || c `elem` (".(=#-[*`\\\"'_/@<>" :: String)
          filterSpecial = filter (not . isSpecial)

      it "parses an empty paragraph" $ do
        "" `shouldParseTo` DocEmpty

      it "parses a simple text paragraph" $ do
        "foo bar baz" `shouldParseTo` DocParagraph "foo bar baz"

      it "accepts markup in text paragraphs" $ do
        "foo /bar/ baz" `shouldParseTo` DocParagraph ("foo " <> DocEmphasis "bar" <> " baz")

      it "preserve all regular characters" $ do
        property $ \xs ->
          let input = filterSpecial xs
           in (not . null) input ==>
                input `shouldParseTo` DocParagraph (DocString input)

      it "separates paragraphs by empty lines" $ do
        unlines
          [ "foo"
          , " \t "
          , "bar"
          ]
          `shouldParseTo` DocParagraph "foo"
          <> DocParagraph "bar"

      context "when a pragraph only contains monospaced text" $ do
        it "turns it into a code block" $ do
          "@foo@" `shouldParseTo` DocCodeBlock "foo"

      context "when a paragraph contains a markdown link" $ do
        it "correctly parses the link" $ do
          "Blah [label](url)"
            `shouldParseTo` DocParagraph ("Blah " <> hyperlink "url" "label")

        context "when the paragraph starts with the markdown link" $ do
          it "correctly parses it as a text paragraph (not a definition list)" $ do
            "[label](url)"
              `shouldParseTo` DocParagraph (hyperlink "url" "label")

          it "can be followed by an other paragraph" $ do
            "[label](url)\n\nfoobar"
              `shouldParseTo` DocParagraph (hyperlink "url" "label")
              <> DocParagraph "foobar"

          context "when paragraph contains additional text" $ do
            it "accepts more text after the link" $ do
              "[label](url) foo bar baz"
                `shouldParseTo` DocParagraph (hyperlink "url" "label" <> " foo bar baz")

            it "accepts a newline right after the markdown link" $ do
              "[label](url)\nfoo bar baz"
                `shouldParseTo` DocParagraph (hyperlink "url" "label" <> " foo bar baz")

            it "can be followed by an other paragraph" $ do
              "[label](url)foo\n\nbar"
                `shouldParseTo` DocParagraph (hyperlink "url" "label" <> "foo")
                <> DocParagraph "bar"

        context "when the link starts on a new line not at the beginning of the paragraph" $ do
          it "correctly parses the link" $ do
            "Bla\n[label](url)"
              `shouldParseTo` DocParagraph ("Bla\n" <> hyperlink "url" "label")

    context "when parsing birdtracks" $ do
      it "parses them as a code block" $ do
        unlines
          [ ">foo"
          , ">bar"
          , ">baz"
          ]
          `shouldParseTo` DocCodeBlock "foo\nbar\nbaz"

      it "ignores leading whitespace" $
        do
          unlines
            [ " >foo"
            , " \t >bar"
            , " >baz"
            ]
          `shouldParseTo` DocCodeBlock "foo\nbar\nbaz"

      it "strips one leading space from each line of the block" $ do
        unlines
          [ "> foo"
          , ">  bar"
          , "> baz"
          ]
          `shouldParseTo` DocCodeBlock "foo\n bar\nbaz"

      it "ignores empty lines when stripping spaces" $ do
        unlines
          [ "> foo"
          , ">"
          , "> bar"
          ]
          `shouldParseTo` DocCodeBlock "foo\n\nbar"

      context "when any non-empty line does not start with a space" $ do
        it "does not strip any spaces" $ do
          unlines
            [ ">foo"
            , ">  bar"
            ]
            `shouldParseTo` DocCodeBlock "foo\n  bar"

      it "ignores nested markup" $ do
        unlines
          [ ">/foo/"
          ]
          `shouldParseTo` DocCodeBlock "/foo/"

      it "treats them as regular text inside text paragraphs" $ do
        unlines
          [ "foo"
          , ">bar"
          ]
          `shouldParseTo` DocParagraph "foo\n>bar"

    context "when parsing code blocks" $ do
      it "accepts a simple code block" $ do
        unlines
          [ "@"
          , "foo"
          , "bar"
          , "baz"
          , "@"
          ]
          `shouldParseTo` DocCodeBlock "foo\nbar\nbaz\n"

      it "ignores trailing whitespace after the opening @" $ do
        unlines
          [ "@   "
          , "foo"
          , "@"
          ]
          `shouldParseTo` DocCodeBlock "foo\n"

      it "rejects code blocks that are not closed" $ do
        unlines
          [ "@"
          , "foo"
          ]
          `shouldParseTo` DocParagraph "@\nfoo"

      it "accepts nested markup" $ do
        unlines
          [ "@"
          , "/foo/"
          , "@"
          ]
          `shouldParseTo` DocCodeBlock (DocEmphasis "foo" <> "\n")

      it "allows to escape the @" $ do
        unlines
          [ "@"
          , "foo"
          , "\\@"
          , "bar"
          , "@"
          ]
          `shouldParseTo` DocCodeBlock "foo\n@\nbar\n"

      it "accepts horizontal space before the @" $ do
        unlines
          [ "     @"
          , "foo"
          , ""
          , "bar"
          , "@"
          ]
          `shouldParseTo` DocCodeBlock "foo\n\nbar\n"

      it "strips a leading space from a @ block if present" $ do
        unlines
          [ " @"
          , " hello"
          , " world"
          , " @"
          ]
          `shouldParseTo` DocCodeBlock "hello\nworld\n"

        unlines
          [ " @"
          , " hello"
          , ""
          , " world"
          , " @"
          ]
          `shouldParseTo` DocCodeBlock "hello\n\nworld\n"

      it "only drops whitespace if there's some before closing @" $ do
        unlines
          [ "@"
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
        unlines
          [ ">>> foo"
          , "bar"
          , "baz"
          ]
          `shouldParseTo` DocExamples [Example "foo" ["bar", "baz"]]

      it "parses consecutive examples" $ do
        unlines
          [ ">>> fib 5"
          , "5"
          , ">>> fib 10"
          , "55"
          ]
          `shouldParseTo` DocExamples
            [ Example "fib 5" ["5"]
            , Example "fib 10" ["55"]
            ]

      it
        ( "requires an example to be separated"
            ++ " from a previous paragraph by an empty line"
        )
        $ do
          "foobar\n\n>>> fib 10\n55"
            `shouldParseTo` DocParagraph "foobar"
            <> DocExamples [Example "fib 10" ["55"]]

      it "parses bird-tracks inside of paragraphs as plain strings" $ do
        let xs = "foo\n>>> bar"
        xs `shouldParseTo` DocParagraph (DocString xs)

      it "skips empty lines in front of an example" $ do
        "\n   \n\n>>> foo" `shouldParseTo` DocExamples [Example "foo" []]

      it "terminates example on empty line" $
        do
          unlines
            [ ">>> foo"
            , "bar"
            , "    "
            , "baz"
            ]
          `shouldParseTo` DocExamples [Example "foo" ["bar"]]
          <> DocParagraph "baz"

      it "parses a <BLANKLINE> result as an empty result" $
        do
          unlines
            [ ">>> foo"
            , "bar"
            , "<BLANKLINE>"
            , "baz"
            ]
          `shouldParseTo` DocExamples [Example "foo" ["bar", "", "baz"]]

      it "accepts unicode in examples" $ do
        ">>> 灼眼\nシャナ" `shouldParseTo` DocExamples [Example "灼眼" ["シャナ"]]

      it "preserves indentation in consecutive example lines" $ do
        unlines
          [ ">>> line 1"
          , ">>>   line 2"
          , ">>> line 3"
          ]
          `shouldParseTo` DocExamples
            [ Example "line 1" []
            , Example "  line 2" []
            , Example "line 3" []
            ]

      it "resets indentation after results" $ do
        unlines
          [ ">>> line 1"
          , "result"
          , ">>>   line 2"
          ]
          `shouldParseTo` DocExamples
            [ Example "line 1" ["result"]
            , Example "line 2" []
            ]

      context "when prompt is prefixed by whitespace" $ do
        it "strips the exact same amount of whitespace from result lines" $ do
          unlines
            [ "   >>> foo"
            , "   bar"
            , "   baz"
            ]
            `shouldParseTo` DocExamples [Example "foo" ["bar", "baz"]]

        it "preserves additional whitespace" $ do
          unlines
            [ "   >>> foo"
            , "    bar"
            ]
            `shouldParseTo` DocExamples [Example "foo" [" bar"]]

        it "keeps original if stripping is not possible" $ do
          unlines
            [ "   >>> foo"
            , " bar"
            ]
            `shouldParseTo` DocExamples [Example "foo" [" bar"]]

    context "when parsing paragraphs nested in lists" $ do
      it "can nest the same type of list" $ do
        "* foo\n\n    * bar"
          `shouldParseTo` DocUnorderedList
            [ DocParagraph "foo"
                <> DocUnorderedList [DocParagraph "bar"]
            ]

      it "can nest another type of list inside" $ do
        "* foo\n\n    1. bar"
          `shouldParseTo` DocUnorderedList
            [ DocParagraph "foo"
                <> DocOrderedList [(1, DocParagraph "bar")]
            ]

      it "can nest a code block inside" $ do
        "* foo\n\n    @foo bar baz@"
          `shouldParseTo` DocUnorderedList
            [ DocParagraph "foo"
                <> DocCodeBlock "foo bar baz"
            ]

        "* foo\n\n    @\n    foo bar baz\n    @"
          `shouldParseTo` DocUnorderedList
            [ DocParagraph "foo"
                <> DocCodeBlock "foo bar baz\n"
            ]

      it "can nest more than one level" $ do
        "* foo\n\n    * bar\n\n        * baz\n        qux"
          `shouldParseTo` DocUnorderedList
            [ DocParagraph "foo"
                <> DocUnorderedList
                  [ DocParagraph "bar"
                      <> DocUnorderedList [DocParagraph "baz\nqux"]
                  ]
            ]

      it "won't fail on not fully indented paragraph" $ do
        "* foo\n\n    * bar\n\n        * qux\nquux"
          `shouldParseTo` DocUnorderedList
            [ DocParagraph "foo"
                <> DocUnorderedList [DocParagraph "bar"]
            , DocParagraph "qux\nquux"
            ]

      it "can nest definition lists" $ do
        "[a]: foo\n\n    [b]: bar\n\n        [c]: baz\n        qux"
          `shouldParseTo` DocDefList
            [
              ( "a"
              , "foo"
                  <> DocDefList
                    [
                      ( "b"
                      , "bar"
                          <> DocDefList [("c", "baz\nqux")]
                      )
                    ]
              )
            ]

      it "can come back to top level with a different list" $ do
        "* foo\n\n    * bar\n\n1. baz"
          `shouldParseTo` DocUnorderedList
            [ DocParagraph "foo"
                <> DocUnorderedList [DocParagraph "bar"]
            ]
          <> DocOrderedList [(1, DocParagraph "baz")]

      it "allows arbitrary initial indent of a list" $
        do
          unlines
            [ "     * foo"
            , "     * bar"
            , ""
            , "         * quux"
            , ""
            , "     * baz"
            ]
          `shouldParseTo` DocUnorderedList
            [ DocParagraph "foo"
            , DocParagraph "bar"
                <> DocUnorderedList [DocParagraph "quux"]
            , DocParagraph "baz"
            ]

      it "definition lists can come back to top level with a different list" $ do
        "[foo]: foov\n\n    [bar]: barv\n\n1. baz"
          `shouldParseTo` DocDefList
            [
              ( "foo"
              , "foov"
                  <> DocDefList [("bar", "barv")]
              )
            ]
          <> DocOrderedList [(1, DocParagraph "baz")]

      it "list order is preserved in presence of nesting + extra text" $ do
        "1. Foo\n\n    > Some code\n\n2. Bar\n\nSome text"
          `shouldParseTo` DocOrderedList
            [ (1, DocParagraph "Foo" <> DocCodeBlock "Some code")
            , (2, DocParagraph "Bar")
            ]
          <> DocParagraph (DocString "Some text")

        "1. Foo\n\n2. Bar\n\nSome text"
          `shouldParseTo` DocOrderedList
            [ (1, DocParagraph "Foo")
            , (2, DocParagraph "Bar")
            ]
          <> DocParagraph (DocString "Some text")

    context "when parsing properties" $ do
      it "can parse a single property" $ do
        "prop> 23 == 23" `shouldParseTo` DocProperty "23 == 23"

      it "can parse multiple subsequent properties" $
        do
          unlines
            [ "prop> 23 == 23"
            , "prop> 42 == 42"
            ]
          `shouldParseTo` DocProperty "23 == 23"
          <> DocProperty "42 == 42"

      it "accepts unicode in properties" $ do
        "prop> 灼眼のシャナ ≡ 愛"
          `shouldParseTo` DocProperty "灼眼のシャナ ≡ 愛"

      it "can deal with whitespace before and after the prop> prompt" $ do
        "  prop>     xs == (reverse $ reverse xs)  "
          `shouldParseTo` DocProperty "xs == (reverse $ reverse xs)"

    context "when parsing unordered lists" $ do
      it "parses a simple list" $
        do
          unlines
            [ " * one"
            , " * two"
            , " * three"
            ]
          `shouldParseTo` DocUnorderedList
            [ DocParagraph "one"
            , DocParagraph "two"
            , DocParagraph "three"
            ]

      it "ignores empty lines between list items" $
        do
          unlines
            [ "* one"
            , ""
            , "* two"
            ]
          `shouldParseTo` DocUnorderedList
            [ DocParagraph "one"
            , DocParagraph "two"
            ]

      it "accepts an empty list item" $ do
        "*" `shouldParseTo` DocUnorderedList [DocParagraph DocEmpty]

      it "accepts multi-line list items" $
        do
          unlines
            [ "* point one"
            , "  more one"
            , "* point two"
            , "more two"
            ]
          `shouldParseTo` DocUnorderedList
            [ DocParagraph "point one\n  more one"
            , DocParagraph "point two\nmore two"
            ]

      it "accepts markup in list items" $ do
        "* /foo/" `shouldParseTo` DocUnorderedList [DocParagraph (DocEmphasis "foo")]

      it "requires empty lines between list and other paragraphs" $
        do
          unlines
            [ "foo"
            , ""
            , "* bar"
            , ""
            , "baz"
            ]
          `shouldParseTo` DocParagraph "foo"
          <> DocUnorderedList [DocParagraph "bar"]
          <> DocParagraph "baz"

    context "when parsing ordered lists" $ do
      it "parses a simple list" $
        do
          unlines
            [ " 1. one"
            , " (1) two"
            , " 3. three"
            ]
          `shouldParseTo` DocOrderedList
            [ (1, DocParagraph "one")
            , (1, DocParagraph "two")
            , (3, DocParagraph "three")
            ]

      it "ignores empty lines between list items" $
        do
          unlines
            [ "1. one"
            , ""
            , "2. two"
            ]
          `shouldParseTo` DocOrderedList
            [ (1, DocParagraph "one")
            , (2, DocParagraph "two")
            ]

      it "accepts an empty list item" $ do
        "1." `shouldParseTo` DocOrderedList [(1, DocParagraph DocEmpty)]

      it "accepts multi-line list items" $
        do
          unlines
            [ "1. point one"
            , "  more one"
            , "1. point two"
            , "more two"
            ]
          `shouldParseTo` DocOrderedList
            [ (1, DocParagraph "point one\n  more one")
            , (1, DocParagraph "point two\nmore two")
            ]

      it "accepts markup in list items" $ do
        "1. /foo/" `shouldParseTo` DocOrderedList [(1, DocParagraph (DocEmphasis "foo"))]

      it "requires empty lines between list and other paragraphs" $
        do
          unlines
            [ "foo"
            , ""
            , "1. bar"
            , ""
            , "baz"
            ]
          `shouldParseTo` DocParagraph "foo"
          <> DocOrderedList [(1, DocParagraph "bar")]
          <> DocParagraph "baz"

    context "when parsing definition lists" $ do
      it "parses a simple list" $
        do
          unlines
            [ " [foo]: one"
            , " [bar]: two"
            , " [baz]: three"
            ]
          `shouldParseTo` DocDefList
            [ ("foo", "one")
            , ("bar", "two")
            , ("baz", "three")
            ]

      it "ignores empty lines between list items" $
        do
          unlines
            [ "[foo]: one"
            , ""
            , "[bar]: two"
            ]
          `shouldParseTo` DocDefList
            [ ("foo", "one")
            , ("bar", "two")
            ]

      it "accepts an empty list item" $ do
        "[foo]:" `shouldParseTo` DocDefList [("foo", DocEmpty)]

      it "accepts multi-line list items" $
        do
          unlines
            [ "[foo]: point one"
            , "  more one"
            , "[bar]: point two"
            , "more two"
            ]
          `shouldParseTo` DocDefList
            [ ("foo", "point one\n  more one")
            , ("bar", "point two\nmore two")
            ]

      it "accepts markup in list items" $ do
        "[foo]: /foo/" `shouldParseTo` DocDefList [("foo", DocEmphasis "foo")]

      it "accepts markup for the label" $ do
        "[/foo/]: bar" `shouldParseTo` DocDefList [(DocEmphasis "foo", "bar")]

      it "requires empty lines between list and other paragraphs" $
        do
          unlines
            [ "foo"
            , ""
            , "[foo]: bar"
            , ""
            , "baz"
            ]
          `shouldParseTo` DocParagraph "foo"
          <> DocDefList [("foo", "bar")]
          <> DocParagraph "baz"

      it "dose not require the colon (deprecated - this will be removed in a future release)" $
        do
          unlines
            [ " [foo] one"
            , " [bar] two"
            , " [baz] three"
            ]
          `shouldParseTo` DocDefList
            [ ("foo", "one")
            , ("bar", "two")
            , ("baz", "three")
            ]

    context "when parsing consecutive paragraphs" $ do
      it "will not capture irrelevant consecutive lists" $ do
        unlines
          [ "   * bullet"
          , ""
          , ""
          , "   - different bullet"
          , ""
          , ""
          , "   (1) ordered"
          , " "
          , "   2. different bullet"
          , "   "
          , "   [cat]: kitten"
          , "   "
          , "   [pineapple]: fruit"
          ]
          `shouldParseTo` DocUnorderedList
            [ DocParagraph "bullet"
            , DocParagraph "different bullet"
            ]
          <> DocOrderedList
            [ (1, DocParagraph "ordered")
            , (2, DocParagraph "different bullet")
            ]
          <> DocDefList
            [ ("cat", "kitten")
            , ("pineapple", "fruit")
            ]

    context "when parsing function documentation headers" $ do
      it "can parse a simple header" $ do
        "= Header 1\nHello."
          `shouldParseTo` (DocHeader (Header 1 "Header 1"))
          <> DocParagraph "Hello."

      it "allow consecutive headers" $ do
        "= Header 1\n== Header 2"
          `shouldParseTo` DocHeader (Header 1 "Header 1")
          <> DocHeader (Header 2 "Header 2")

      it "accepts markup in the header" $ do
        "= /Header/ __1__\nFoo"
          `shouldParseTo` DocHeader (Header 1 (DocEmphasis "Header" <> " " <> DocBold "1"))
          <> DocParagraph "Foo"
