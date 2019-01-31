{-# LANGUAGE OverloadedStrings #-}
module Haddock.Backends.Hyperlinker.ParserSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import GHC           ( runGhc, getSessionDynFlags )
import DynFlags      ( CompilerInfo, DynFlags )
import SysTools.Info ( getCompilerInfo' )
import Control.Monad.IO.Class

import Data.String     ( fromString )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS 

import Haddock (getGhcDirs)
import Haddock.Backends.Hyperlinker.Parser
import Haddock.Backends.Hyperlinker.Types

withDynFlags :: ((DynFlags, CompilerInfo) -> IO ()) -> IO ()
withDynFlags cont = do
  libDir <- fmap snd (getGhcDirs [])
  runGhc (Just libDir) $ do
    dflags <- getSessionDynFlags
    cinfo <- liftIO $ getCompilerInfo' dflags
    liftIO $ cont (dflags, cinfo)


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
noGhcRewrite ('\t':_) = False        -- GHC replaces tabs with 8 spaces
noGhcRewrite ('\r':_) = False
noGhcRewrite ('\f':_) = False
noGhcRewrite ('\v':_) = False
noGhcRewrite (' ':'\n':_) = False    -- GHC strips whitespace on empty lines
noGhcRewrite (_:s) = noGhcRewrite s
noGhcRewrite "" = True

instance Arbitrary NoGhcRewrite where
  arbitrary = fmap NoGhcRewrite (arbitrary `suchThat` noGhcRewrite)
  shrink (NoGhcRewrite src) = [ NoGhcRewrite shrunk
                              | shrunk <- shrink src
                              , noGhcRewrite shrunk
                              ]


parseSpec :: Spec
parseSpec = around withDynFlags $ do

    it "is total" $ \(dflags, cinfo) ->
        property $ \src -> length (parse cinfo dflags "" (fromString src)) `shouldSatisfy` (>= 0)

    it "retains file layout" $ \(dflags, cinfo) ->
        property $ \(NoGhcRewrite src) ->
          let orig = fromString src
              lexed = BS.concat (map tkValue (parse cinfo dflags "" orig))
          in lexed == orig 

    context "when parsing single-line comments" $ do

        it "should ignore content until the end of line" $ \(dflags, cinfo) ->
            shouldParseTo
                "-- some very simple comment\nidentifier"
                [TkComment, TkSpace, TkIdentifier]
                cinfo
                dflags

        it "should allow endline escaping" $ \(dflags, cinfo) ->
            shouldParseTo
                "#define first line\\\nsecond line\\\nand another one"
                [TkCpp]
                cinfo
                dflags

    context "when parsing multi-line comments" $ do

        it "should support nested comments" $ \(dflags, cinfo) ->
            shouldParseTo
                "{- comment {- nested -} still comment -} {- next comment -}"
                [TkComment, TkSpace, TkComment]
                cinfo
                dflags

        it "should distinguish compiler pragma" $ \(dflags, cinfo) ->
            shouldParseTo
                "{- comment -}{-# LANGUAGE GADTs #-}{- comment -}"
                [TkComment, TkPragma, TkComment]
                cinfo
                dflags

    it "should recognize preprocessor directives" $ \(dflags, cinfo) -> do
            shouldParseTo
                "\n#define foo bar"
                [TkCpp]
                cinfo
                dflags
            shouldParseTo
                "x # y"
                [TkIdentifier, TkSpace, TkOperator, TkSpace,TkIdentifier]
                cinfo
                dflags

    it "should distinguish basic language constructs" $ \(dflags, cinfo) -> do
        
        shouldParseTo
            "(* 2) <$> (\"abc\", foo)"
            [ TkSpecial, TkOperator, TkSpace, TkNumber, TkSpecial
            , TkSpace, TkOperator, TkSpace
            , TkSpecial, TkString, TkSpecial, TkSpace, TkIdentifier, TkSpecial
            ]
            cinfo
            dflags
            
        shouldParseTo
            "let foo' = foo in foo' + foo'"
            [ TkKeyword, TkSpace, TkIdentifier
            , TkSpace, TkGlyph, TkSpace
            , TkIdentifier, TkSpace, TkKeyword, TkSpace
            , TkIdentifier, TkSpace, TkOperator, TkSpace, TkIdentifier
            ]
            cinfo
            dflags
        
        shouldParseTo
            "square x = y^2 where y = x"
            [ TkIdentifier, TkSpace, TkIdentifier
            , TkSpace, TkGlyph, TkSpace
            , TkIdentifier, TkOperator, TkNumber
            , TkSpace, TkKeyword, TkSpace
            , TkIdentifier, TkSpace, TkGlyph, TkSpace, TkIdentifier
            ]
            cinfo
            dflags

    it "should parse do-notation syntax" $ \(dflags, cinfo) -> do
        shouldParseTo
            "do { foo <- getLine; putStrLn foo }"
            [ TkKeyword, TkSpace, TkSpecial, TkSpace
            , TkIdentifier, TkSpace, TkGlyph, TkSpace
            , TkIdentifier, TkSpecial, TkSpace
            , TkIdentifier, TkSpace, TkIdentifier, TkSpace, TkSpecial
            ]
            cinfo
            dflags

        shouldParseTo
            (fromString $ unlines
                [ "do"
                , "    foo <- getLine"
                , "    putStrLn foo"
                ])
            [ TkKeyword, TkSpace, TkIdentifier
            , TkSpace, TkGlyph, TkSpace, TkIdentifier, TkSpace
            , TkIdentifier, TkSpace, TkIdentifier, TkSpace
            ]
            cinfo
            dflags
  where
    shouldParseTo :: ByteString -> [TokenType] -> CompilerInfo -> DynFlags -> Expectation
    shouldParseTo str tokens cinfo dflags = [ tkType tok
                                            | tok <- parse cinfo dflags "" str
                                            , not (BS.null (tkValue tok)) ] `shouldBe` tokens
