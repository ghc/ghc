-- | Functions and GHC-specific instances for docstrings.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Binary HsDocString
-- Outputable HsDocString

module GHC.Hs.DocString
  ( LHsDocString
  , HsDocString(..)
  , HsDocStringDecorator(..)
  , HsDocStringChunk(..)
  , LHsDocStringChunk
  , isEmptyDocString
  , unpackHDSC
  , mkHsDocStringChunk
  , mkHsDocStringChunkUtf8ByteString
  , pprHsDocString
  , pprHsDocStrings
  , mkGeneratedHsDocStringGhc
  , docStringChunks
  , renderHsDocString
  , renderHsDocStrings
  , renderHsDocStringText
  , renderHsDocStringsText
  , exactPrintHsDocString
  , pprWithDocString
  , printDecorator
  -- * GHC pass conversion
  , rnHsDocString
  , tcHsDocString
  ) where

import GHC.Prelude

import GHC.Utils.Encoding
import GHC.Utils.Outputable as Outputable hiding ((<>))
import GHC.Types.SrcLoc

import GHC.Hs.Extension.Instances ()
import GHC.Hs.Extension.Pass (GhcPass, GhcPs, GhcRn, GhcTc)

import Language.Haskell.Syntax.Doc
import Language.Haskell.Syntax.Extension

import Data.Data
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (intercalate)
import qualified Data.Text as T

type LHsDocString pass = XRec pass (HsDocString pass)

deriving instance
  ( Data pass
  , Data (XMultiLineDocString pass)
  , Data (XNestedDocString pass)
  , Data (XGeneratedDocString pass)
  , Data (LHsDocStringChunk pass)
  , Data (XXHsDocString pass)
  ) => Data (HsDocString pass)

instance (Eq (LHsDocStringChunk pass), XXHsDocString pass ~ DataConCantHappen) => Eq (HsDocString pass) where
  MultiLineDocString _ dec xs == MultiLineDocString _ dec' xs' = dec == dec' && xs == xs'
  NestedDocString _ dec x == NestedDocString _ dec' x' = dec == dec' && x == x'
  GeneratedDocString _ x == GeneratedDocString _ x' = x == x'
  _ == _ = False

-- | Annotate a pretty printed thing with its doc.
-- The docstring comes after if it is 'HsDocStringPrevious'.
-- Otherwise it comes before.
-- Note - we convert MultiLineDocString HsDocStringPrevious to HsDocStringNext
-- because we can't control if something else will be pretty printed on the same line.
pprWithDocString :: HsDocString (GhcPass p) -> SDoc -> SDoc
pprWithDocString (MultiLineDocString x HsDocStringPrevious ds) sd =
  pprWithDocString (MultiLineDocString x HsDocStringNext ds) sd
pprWithDocString doc@(NestedDocString _ HsDocStringPrevious _) sd = sd <+> pprHsDocString doc
pprWithDocString doc sd = pprHsDocString doc $+$ sd

printDecorator :: HsDocStringDecorator -> String
printDecorator HsDocStringNext = "|"
printDecorator HsDocStringPrevious = "^"
printDecorator (HsDocStringNamed n) = '$':n
printDecorator (HsDocStringGroup n) = replicate n '*'

mkGeneratedHsDocStringGhc :: String -> HsDocString (GhcPass p)
mkGeneratedHsDocStringGhc = mkGeneratedHsDocString noExtField . mkHsDocStringChunk

mkHsDocStringChunk :: String -> HsDocStringChunk
mkHsDocStringChunk = HsDocStringChunk . utf8EncodeByteString

unpackHDSC :: HsDocStringChunk -> String
unpackHDSC (HsDocStringChunk bs) = utf8DecodeByteString bs

isEmptyDocString :: HsDocString (GhcPass p) -> Bool
isEmptyDocString (MultiLineDocString _ _ xs) = all (nullHDSC . unLoc) xs
isEmptyDocString (NestedDocString _ _ s) = nullHDSC $ unLoc s
isEmptyDocString (GeneratedDocString _ x) = nullHDSC x

docStringChunks :: HsDocString (GhcPass p) -> [LHsDocStringChunk (GhcPass p)]
docStringChunks (MultiLineDocString _ _ (x:|xs)) = x:xs
docStringChunks (NestedDocString _ _ x) = [x]
docStringChunks (GeneratedDocString _ x) = [L (GeneratedSrcSpan UnhelpfulGenerated) x]

-- | Pretty print with decorators, exactly as the user wrote it.
pprHsDocString :: HsDocString (GhcPass p) -> SDoc
pprHsDocString = text . exactPrintHsDocString

pprHsDocStrings :: [HsDocString (GhcPass p)] -> SDoc
pprHsDocStrings = text . intercalate "\n\n" . map exactPrintHsDocString

-- | Pretty print with decorators, exactly as the user wrote it.
exactPrintHsDocString :: HsDocString (GhcPass p) -> String
exactPrintHsDocString (MultiLineDocString _ dec (x :| xs))
  = unlines' $ ("-- " ++ printDecorator dec ++ unpackHDSC (unLoc x))
            : map (\y -> "--" ++ unpackHDSC (unLoc y)) xs
exactPrintHsDocString (NestedDocString _ dec (L _ s))
  = "{-" ++ printDecorator dec ++ unpackHDSC s ++ "-}"
exactPrintHsDocString (GeneratedDocString _ x) = case lines (unpackHDSC x) of
  [] -> ""
  (x:xs) -> unlines' $ ( "-- |" ++ x)
                    : map (\y -> "--"++y) xs

-- | Don't add a newline to a single string.
unlines' :: [String] -> String
unlines' = intercalate "\n"

-- | Just get the docstring, without any decorators
-- Separates docstrings using "\n\n", which is how haddock likes to render them
renderHsDocStrings :: [HsDocString (GhcPass p)] -> String
renderHsDocStrings = T.unpack . renderHsDocStringsText

-- | Just get the docstrings as 'Text', without any decorators.
-- Separates docstrings using "\n\n", which is how haddock likes to render them.
renderHsDocStringsText :: [HsDocString (GhcPass p)] -> T.Text
renderHsDocStringsText = T.intercalate (T.cons '\n' (T.singleton '\n')) . map renderHsDocStringText

rnHsDocString :: HsDocString GhcPs -> HsDocString GhcRn
rnHsDocString (MultiLineDocString x dec ys) = MultiLineDocString x dec ys
rnHsDocString (NestedDocString    x dec y ) = NestedDocString    x dec y
rnHsDocString (GeneratedDocString x     y ) = GeneratedDocString x     y

tcHsDocString :: HsDocString GhcRn -> HsDocString GhcTc
tcHsDocString (MultiLineDocString x dec ys) = MultiLineDocString x dec ys
tcHsDocString (NestedDocString    x dec y ) = NestedDocString    x dec y
tcHsDocString (GeneratedDocString x     y ) = GeneratedDocString x     y
