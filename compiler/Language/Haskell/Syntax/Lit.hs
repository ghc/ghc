{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- See Note [Language.Haskell.Syntax.* Hierarchy] for why not GHC.Hs.*

-- | Source-language literals
module Language.Haskell.Syntax.Lit where

import Language.Haskell.Syntax.Extension

import GHC.Types.SourceText (IntegralLit, FractionalLit, SourceText)

import GHC.Data.FastString (FastString, lexicalCompareFS)

import Data.ByteString (ByteString)
import Data.Data hiding ( Fixity )
import Data.Bool
import Data.Ord
import Data.Eq
import Data.Char
import Prelude (Integer)

{-
************************************************************************
*                                                                      *
\subsection[HsLit]{Literals}
*                                                                      *
************************************************************************
-}

-- Note [Literal source text] in "GHC.Types.SourceText" for SourceText fields in
-- the following
-- Note [Trees That Grow] in "Language.Haskell.Syntax.Extension" for the Xxxxx
-- fields in the following
-- | Haskell Literal
data HsLit x
  = HsChar (XHsChar x) {- SourceText -} Char
      -- ^ Character
  | HsCharPrim (XHsCharPrim x) {- SourceText -} Char
      -- ^ Unboxed character
  | HsString (XHsString x) {- SourceText -} FastString
      -- ^ String
  | HsStringPrim (XHsStringPrim x) {- SourceText -} !ByteString
      -- ^ Packed bytes
  | HsNatural (XHsNatural x)  IntegralLit
      -- ^ Genuinely a @Natural@; used in types only
  | HsDouble (XHsDouble x)  FractionalLit
      -- ^ Genuinely a @Double@; used in type errors only
  | HsInt (XHsInt x)  IntegralLit
      -- ^ Genuinely an Int; arises from
      -- "GHC.Tc.Deriv.Generate", and from TRANSLATION
  | HsIntPrim (XHsIntPrim x) {- SourceText -} Integer
      -- ^ literal @Int#@
  | HsWordPrim (XHsWordPrim x) {- SourceText -} Integer
      -- ^ literal @Word#@
  | HsInt8Prim (XHsInt8Prim x) {- SourceText -} Integer
      -- ^ literal @Int8#@
  | HsInt16Prim (XHsInt16Prim x) {- SourceText -} Integer
      -- ^ literal @Int16#@
  | HsInt32Prim (XHsInt32Prim x) {- SourceText -} Integer
      -- ^ literal @Int32#@
  | HsInt64Prim (XHsInt64Prim x) {- SourceText -} Integer
      -- ^ literal @Int64#@
  | HsWord8Prim (XHsWord8Prim x) {- SourceText -} Integer
      -- ^ literal @Word8#@
  | HsWord16Prim (XHsWord16Prim x) {- SourceText -} Integer
      -- ^ literal @Word16#@
  | HsWord32Prim (XHsWord32Prim x) {- SourceText -} Integer
      -- ^ literal @Word32#@
  | HsWord64Prim (XHsWord64Prim x) {- SourceText -} Integer
      -- ^ literal @Word64#@
  | HsFloatPrim (XHsFloatPrim x)   FractionalLit
      -- ^ Unboxed Float
  | HsDoublePrim (XHsDoublePrim x) FractionalLit
      -- ^ Unboxed Double
  | XLit !(XXLit x)

instance (Eq (XXLit x)) => Eq (HsLit x) where
  HsChar _ x1       == HsChar _ x2       = x1 == x2
  HsChar{}          == _                 = False
  HsCharPrim _ x1   == HsCharPrim _ x2   = x1 == x2
  HsCharPrim{}      == _                 = False
  HsString _ x1     == HsString _ x2     = x1 == x2
  HsString{}        == _                 = False
  HsStringPrim _ x1 == HsStringPrim _ x2 = x1 == x2
  HsStringPrim{}    == _                 = False
  HsNatural _ x1    == HsNatural _ x2    = x1 == x2
  HsNatural{}       == _                 = False
  HsDouble _ x1     == HsDouble _ x2     = x1 == x2
  HsDouble{}        == _                 = False
  HsInt _ x1        == HsInt _ x2        = x1 == x2
  HsInt{}           == _                 = False
  HsIntPrim _ x1    == HsIntPrim _ x2    = x1 == x2
  HsIntPrim{}       == _                 = False
  HsWordPrim _ x1   == HsWordPrim _ x2   = x1 == x2
  HsWordPrim{}      == _                 = False
  HsInt8Prim _ x1   == HsInt8Prim _ x2   = x1 == x2
  HsInt8Prim{}      == _                 = False
  HsInt16Prim _ x1  == HsInt16Prim _ x2  = x1 == x2
  HsInt16Prim{}     == _                 = False
  HsInt32Prim _ x1  == HsInt32Prim _ x2  = x1 == x2
  HsInt32Prim{}     == _                 = False
  HsInt64Prim _ x1  == HsInt64Prim _ x2  = x1 == x2
  HsInt64Prim{}     == _                 = False
  HsWord8Prim _ x1  == HsWord8Prim _ x2  = x1 == x2
  HsWord8Prim{}     == _                 = False
  HsWord16Prim _ x1 == HsWord16Prim _ x2 = x1 == x2
  HsWord16Prim{}    == _                 = False
  HsWord32Prim _ x1 == HsWord32Prim _ x2 = x1 == x2
  HsWord32Prim{}    == _                 = False
  HsWord64Prim _ x1 == HsWord64Prim _ x2 = x1 == x2
  HsWord64Prim{}    == _                 = False
  HsFloatPrim _ x1  == HsFloatPrim _ x2  = x1 == x2
  HsFloatPrim{}     == _                 = False
  HsDoublePrim _ x1 == HsDoublePrim _ x2 = x1 == x2
  HsDoublePrim{}    == _                 = False
  XLit x1           == XLit x2           = x1 == x2
  XLit{}            == _                 = False

-- | Haskell Overloaded Literal
data HsOverLit p
  = OverLit {
      ol_ext :: (XOverLit p),
      ol_val :: OverLitVal}

  | XOverLit
      !(XXOverLit p)

-- Note [Literal source text] in "GHC.Types.SourceText" for SourceText fields in
-- the following
-- | Overloaded Literal Value
data OverLitVal
  = HsIntegral   !IntegralLit            -- ^ Integer-looking literals;
  | HsFractional !FractionalLit          -- ^ Frac-looking literals
  | HsIsString   !SourceText !FastString -- ^ String-looking literals
  deriving Data

instance Eq OverLitVal where
  HsIntegral   i1 == HsIntegral   i2 = i1 == i2
  HsIntegral{}    == _               = False
  HsFractional f1 == HsFractional f2 = f1 == f2
  HsFractional{}  == _               = False
  HsIsString _ s1 == HsIsString _ s2 = s1 == s2
  HsIsString{}    == _               = False

instance Ord OverLitVal where
  -- HsIntegral
  HsIntegral i1 `compare` HsIntegral i2  = i1 `compare` i2
  HsIntegral{}  `compare` HsFractional{} = LT
  HsIntegral{}  `compare` HsIsString{}   = LT
  -- HsFractional
  HsFractional{}  `compare` HsIntegral{}    = GT
  HsFractional f1 `compare` HsFractional f2 = f1 `compare` f2
  HsFractional{}  `compare` HsIsString{}    = LT
  -- HsIsString
  HsIsString{}    `compare` HsIntegral{}    = GT
  HsIsString{}    `compare` HsFractional{}  = GT
  HsIsString _ s1 `compare` HsIsString _ s2 = s1 `lexicalCompareFS` s2
