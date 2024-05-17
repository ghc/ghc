{-# LANGUAGE OverloadedStrings #-}

module Haddock.Backends.Hyperlinker.ParserSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.IO.Class
import GHC (getSessionDynFlags, runGhc)
import GHC.Driver.Session (DynFlags)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.String (fromString)

import Haddock (getGhcDirs)
import Haddock.Backends.Hyperlinker.Parser
import Haddock.Backends.Hyperlinker.Types

withDynFlags :: (DynFlags -> IO ()) -> IO ()
withDynFlags cont = do
  libDir <- fmap snd (getGhcDirs [])
  runGhc libDir $ do
    dflags <- getSessionDynFlags
    liftIO $ cont dflags

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parse" parseSpec

-- | Defined for its instance of 'Arbitrary'. Represents strings that, when
-- considered as GHC source, won't be rewritten.
newtype NoGhcRewrite = NoGhcRewrite String deriving (Show, Eq)

-- | Filter out strings where GHC would replace/remove some characters during
-- lexing.
noGhcRewrite :: String -> Bool
noGhcRewrite ('\t' : _) = False -- GHC replaces tabs with 8 spaces
noGhcRewrite ('\r' : _) = False
noGhcRewrite ('\f' : _) = False
noGhcRewrite ('\v' : _) = False
noGhcRewrite (' ' : '\n' : _) = False -- GHC strips whitespace on empty lines
noGhcRewrite (_ : s) = noGhcRewrite s
noGhcRewrite "" = True

instance Arbitrary NoGhcRewrite where
  arbitrary = fmap NoGhcRewrite (arbitrary `suchThat` noGhcRewrite)
  shrink (NoGhcRewrite src) =
    [ NoGhcRewrite shrunk
    | shrunk <- shrink src
    , noGhcRewrite shrunk
    ]

parseSpec :: Spec
parseSpec = around withDynFlags $ do
  it "is total" $ \dflags ->
    property $ \src -> length (parse dflags "" (fromString src)) `shouldSatisfy` (>= 0)

  it "retains file layout" $ \dflags ->
    property $ \(NoGhcRewrite src) ->
      let orig = fromString src
          lexed = BS.concat (map tkValue (parse dflags "" orig))
       in lexed == orig

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
      [TkCpp]
      dflags
    shouldParseTo
      "x # y"
      [TkIdentifier, TkSpace, TkOperator, TkSpace, TkIdentifier]
      dflags

  it "should distinguish basic language constructs" $ \dflags -> do
    shouldParseTo
      "(* 2) <$> (\"abc\", foo)"
      [ TkSpecial
      , TkOperator
      , TkSpace
      , TkNumber
      , TkSpecial
      , TkSpace
      , TkOperator
      , TkSpace
      , TkSpecial
      , TkString
      , TkSpecial
      , TkSpace
      , TkIdentifier
      , TkSpecial
      ]
      dflags

    shouldParseTo
      "let foo' = foo in foo' + foo'"
      [ TkKeyword
      , TkSpace
      , TkIdentifier
      , TkSpace
      , TkGlyph
      , TkSpace
      , TkIdentifier
      , TkSpace
      , TkKeyword
      , TkSpace
      , TkIdentifier
      , TkSpace
      , TkOperator
      , TkSpace
      , TkIdentifier
      ]
      dflags

    shouldParseTo
      "square x = y^2 where y = x"
      [ TkIdentifier
      , TkSpace
      , TkIdentifier
      , TkSpace
      , TkGlyph
      , TkSpace
      , TkIdentifier
      , TkOperator
      , TkNumber
      , TkSpace
      , TkKeyword
      , TkSpace
      , TkIdentifier
      , TkSpace
      , TkGlyph
      , TkSpace
      , TkIdentifier
      ]
      dflags

  it "should parse do-notation syntax" $ \dflags -> do
    shouldParseTo
      "do { foo <- getLine; putStrLn foo }"
      [ TkKeyword
      , TkSpace
      , TkSpecial
      , TkSpace
      , TkIdentifier
      , TkSpace
      , TkGlyph
      , TkSpace
      , TkIdentifier
      , TkSpecial
      , TkSpace
      , TkIdentifier
      , TkSpace
      , TkIdentifier
      , TkSpace
      , TkSpecial
      ]
      dflags

    shouldParseTo
      ( fromString $
          unlines
            [ "do"
            , "    foo <- getLine"
            , "    putStrLn foo"
            ]
      )
      [ TkKeyword
      , TkSpace
      , TkIdentifier
      , TkSpace
      , TkGlyph
      , TkSpace
      , TkIdentifier
      , TkSpace
      , TkIdentifier
      , TkSpace
      , TkIdentifier
      , TkSpace
      ]
      dflags
  where
    shouldParseTo :: ByteString -> [TokenType] -> DynFlags -> Expectation
    shouldParseTo str tokens dflags =
      [ tkType tok
      | tok <- parse dflags "" str
      , not (BS.null (tkValue tok))
      ]
        `shouldBe` tokens
