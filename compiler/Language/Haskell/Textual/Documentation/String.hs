-- | An exactprintable structure for docstrings
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.Textual.Documentation.String
  ( LHsDocString
  , HsDocString(..)
  , HsDocStringDecorator(..)
  , HsDocStringChunk(..)
  , LHsDocStringChunk
  , isEmptyDocString
  , unpackHDSC
  , mkHsDocStringChunk
  , mkHsDocStringChunkUtf8ByteString
  , mkGeneratedHsDocString
  , docStringChunks
  , renderHsDocString
  , renderHsDocStrings
  , exactPrintHsDocString
  , printDecorator
  ) where

import Prelude

import Control.DeepSeq

import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SBS
import Data.Data
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (intercalate)

import Language.Haskell.Textual.Location
import Language.Haskell.Textual.UTF8

type LHsDocString = Located HsDocString

-- | Haskell Documentation String
--
-- Rich structure to support exact printing
-- The location around each chunk doesn't include the decorators
data HsDocString
  = MultiLineDocString !HsDocStringDecorator !(NonEmpty LHsDocStringChunk)
     -- ^ The first chunk is preceded by "-- <decorator>" and each following chunk is preceded by "--"
     -- Example: -- | This is a docstring for 'foo'. It is the line with the decorator '|' and is always included
     --          -- This continues that docstring and is the second element in the NonEmpty list
     --          foo :: a -> a
  | NestedDocString !HsDocStringDecorator LHsDocStringChunk
     -- ^ The docstring is preceded by "{-<decorator>" and followed by "-}"
     -- The chunk contains balanced pairs of '{-' and '-}'
  | GeneratedDocString HsDocStringChunk
     -- ^ A docstring generated either internally or via TH
     -- Pretty printed with the '-- |' decorator
     -- This is because it may contain unbalanced pairs of '{-' and '-}' and
     -- not form a valid 'NestedDocString'
  deriving (Eq, Data, Show)

instance NFData HsDocString where
  rnf (MultiLineDocString a b) = rnf a `seq` rnf b
  rnf (NestedDocString a b) = rnf a `seq` rnf b
  rnf (GeneratedDocString a) = rnf a

data HsDocStringDecorator
  = HsDocStringNext -- ^ '|' is the decorator
  | HsDocStringPrevious -- ^ '^' is the decorator
  | HsDocStringNamed !String -- ^ '$<string>' is the decorator
  | HsDocStringGroup !Int -- ^ The decorator is the given number of '*'s
  deriving (Eq, Ord, Show, Data)

instance NFData HsDocStringDecorator where
  rnf HsDocStringNext = ()
  rnf HsDocStringPrevious = ()
  rnf (HsDocStringNamed x) = rnf x
  rnf (HsDocStringGroup x) = rnf x

printDecorator :: HsDocStringDecorator -> String
printDecorator HsDocStringNext = "|"
printDecorator HsDocStringPrevious = "^"
printDecorator (HsDocStringNamed n) = '$':n
printDecorator (HsDocStringGroup n) = replicate n '*'

type LHsDocStringChunk = Located HsDocStringChunk

-- | A contiguous chunk of documentation
newtype HsDocStringChunk = HsDocStringChunk TextUTF8
  deriving stock (Eq,Ord,Data, Show)
  deriving newtype (NFData)

mkHsDocStringChunk :: String -> HsDocStringChunk
mkHsDocStringChunk = HsDocStringChunk . encodeUTF8

-- | Create a 'HsDocString' from a UTF8-encoded 'ByteString'.
mkHsDocStringChunkUtf8ByteString :: ByteString -> HsDocStringChunk
mkHsDocStringChunkUtf8ByteString = HsDocStringChunk . unsafeFromShortByteString . SBS.toShort

unpackHDSC :: HsDocStringChunk -> String
unpackHDSC (HsDocStringChunk utf8) = decodeUTF8 utf8

nullHDSC :: HsDocStringChunk -> Bool
nullHDSC (HsDocStringChunk utf8) = mempty == utf8

mkGeneratedHsDocString :: String -> HsDocString
mkGeneratedHsDocString = GeneratedDocString . mkHsDocStringChunk

isEmptyDocString :: HsDocString -> Bool
isEmptyDocString (MultiLineDocString _ xs) = all (nullHDSC . unLoc) xs
isEmptyDocString (NestedDocString _ s) = nullHDSC $ unLoc s
isEmptyDocString (GeneratedDocString x) = nullHDSC x

docStringChunks :: HsDocString -> [LHsDocStringChunk]
docStringChunks (MultiLineDocString _ (x:|xs)) = x:xs
docStringChunks (NestedDocString _ x) = [x]
docStringChunks (GeneratedDocString x) = [L (UnhelpfulSpan UnhelpfulGenerated) x]

-- | Pretty print with decorators, exactly as the user wrote it
exactPrintHsDocString :: HsDocString -> String
exactPrintHsDocString (MultiLineDocString dec (x :| xs))
  = unlines' $ ("-- " ++ printDecorator dec ++ unpackHDSC (unLoc x))
            : map (\x -> "--" ++ unpackHDSC (unLoc x)) xs
exactPrintHsDocString (NestedDocString dec (L _ s))
  = "{-" ++ printDecorator dec ++ unpackHDSC s ++ "-}"
exactPrintHsDocString (GeneratedDocString x) = case lines (unpackHDSC x) of
  [] -> ""
  (x:xs) -> unlines' $ ( "-- |" ++ x)
                    : map (\y -> "--"++y) xs

-- | Just get the docstring, without any decorators
renderHsDocString :: HsDocString -> String
renderHsDocString (MultiLineDocString _ (x :| xs)) = unlines' $ map (unpackHDSC . unLoc) (x:xs)
renderHsDocString (NestedDocString _ ds) = unpackHDSC $ unLoc ds
renderHsDocString (GeneratedDocString x) = unpackHDSC x

-- | Don't add a newline to a single string
unlines' :: [String] -> String
unlines' = intercalate "\n"

-- | Just get the docstring, without any decorators
-- Separates docstrings using "\n\n", which is how haddock likes to render them
renderHsDocStrings :: [HsDocString] -> String
renderHsDocStrings = intercalate "\n\n" . map renderHsDocString
