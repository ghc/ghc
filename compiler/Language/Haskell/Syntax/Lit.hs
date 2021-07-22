
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
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

import GHC.Prelude

import GHC.Types.Basic (PprPrec(..), topPrec )
import GHC.Types.SourceText
import GHC.Core.Type
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.FastString
import Language.Haskell.Syntax.Extension

import Data.ByteString (ByteString)
import Data.Data hiding ( Fixity )

{-
************************************************************************
*                                                                      *
\subsection[HsLit]{Literals}
*                                                                      *
************************************************************************
-}

-- Note [Literal source text] in GHC.Types.Basic for SourceText fields in
-- the following
-- Note [Trees That Grow] in Language.Haskell.Syntax.Extension for the Xxxxx
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
  | HsInt (XHsInt x)  IntegralLit
      -- ^ Genuinely an Int; arises from
      -- "GHC.Tc.Deriv.Generate", and from TRANSLATION
  | HsIntPrim (XHsIntPrim x) {- SourceText -} Integer
      -- ^ literal @Int#@
  | HsWordPrim (XHsWordPrim x) {- SourceText -} Integer
      -- ^ literal @Word#@
  | HsInt64Prim (XHsInt64Prim x) {- SourceText -} Integer
      -- ^ literal @Int64#@
  | HsWord64Prim (XHsWord64Prim x) {- SourceText -} Integer
      -- ^ literal @Word64#@
  | HsInteger (XHsInteger x) {- SourceText -} Integer Type
      -- ^ Genuinely an integer; arises only
      -- from TRANSLATION (overloaded
      -- literals are done with HsOverLit)
  | HsRat (XHsRat x)  FractionalLit Type
      -- ^ Genuinely a rational; arises only from
      -- TRANSLATION (overloaded literals are
      -- done with HsOverLit)
  | HsFloatPrim (XHsFloatPrim x)   FractionalLit
      -- ^ Unboxed Float
  | HsDoublePrim (XHsDoublePrim x) FractionalLit
      -- ^ Unboxed Double

  | XLit !(XXLit x)

instance Eq (HsLit x) where
  (HsChar _ x1)       == (HsChar _ x2)       = x1==x2
  (HsCharPrim _ x1)   == (HsCharPrim _ x2)   = x1==x2
  (HsString _ x1)     == (HsString _ x2)     = x1==x2
  (HsStringPrim _ x1) == (HsStringPrim _ x2) = x1==x2
  (HsInt _ x1)        == (HsInt _ x2)        = x1==x2
  (HsIntPrim _ x1)    == (HsIntPrim _ x2)    = x1==x2
  (HsWordPrim _ x1)   == (HsWordPrim _ x2)   = x1==x2
  (HsInt64Prim _ x1)  == (HsInt64Prim _ x2)  = x1==x2
  (HsWord64Prim _ x1) == (HsWord64Prim _ x2) = x1==x2
  (HsInteger _ x1 _)  == (HsInteger _ x2 _)  = x1==x2
  (HsRat _ x1 _)      == (HsRat _ x2 _)      = x1==x2
  (HsFloatPrim _ x1)  == (HsFloatPrim _ x2)  = x1==x2
  (HsDoublePrim _ x1) == (HsDoublePrim _ x2) = x1==x2
  _                   == _                   = False

-- | Haskell Overloaded Literal
data HsOverLit p
  = OverLit {
      ol_ext :: (XOverLit p),
      ol_val :: OverLitVal}

  | XOverLit
      !(XXOverLit p)

-- Note [Literal source text] in GHC.Types.Basic for SourceText fields in
-- the following
-- | Overloaded Literal Value
data OverLitVal
  = HsIntegral   !IntegralLit            -- ^ Integer-looking literals;
  | HsFractional !FractionalLit          -- ^ Frac-looking literals
  | HsIsString   !SourceText !FastString -- ^ String-looking literals
  deriving Data

negateOverLitVal :: OverLitVal -> OverLitVal
negateOverLitVal (HsIntegral i) = HsIntegral (negateIntegralLit i)
negateOverLitVal (HsFractional f) = HsFractional (negateFractionalLit f)
negateOverLitVal _ = panic "negateOverLitVal: argument is not a number"

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module GHC.HsToCore.Match.Literal)
instance (Eq (XXOverLit p)) => Eq (HsOverLit p) where
  (OverLit _ val1) == (OverLit _ val2) = val1 == val2
  (XOverLit  val1) == (XOverLit  val2) = val1 == val2
  _ == _ = panic "Eq HsOverLit"

instance Eq OverLitVal where
  (HsIntegral   i1)   == (HsIntegral   i2)   = i1 == i2
  (HsFractional f1)   == (HsFractional f2)   = f1 == f2
  (HsIsString _ s1)   == (HsIsString _ s2)   = s1 == s2
  _                   == _                   = False

instance (Ord (XXOverLit p)) => Ord (HsOverLit p) where
  compare (OverLit _ val1)  (OverLit _ val2) = val1 `compare` val2
  compare (XOverLit  val1)  (XOverLit  val2) = val1 `compare` val2
  compare _ _ = panic "Ord HsOverLit"

instance Ord OverLitVal where
  compare (HsIntegral i1)     (HsIntegral i2)     = i1 `compare` i2
  compare (HsIntegral _)      (HsFractional _)    = LT
  compare (HsIntegral _)      (HsIsString _ _)    = LT
  compare (HsFractional f1)   (HsFractional f2)   = f1 `compare` f2
  compare (HsFractional _)    (HsIntegral   _)    = GT
  compare (HsFractional _)    (HsIsString _ _)    = LT
  compare (HsIsString _ s1)   (HsIsString _ s2)   = s1 `lexicalCompareFS` s2
  compare (HsIsString _ _)    (HsIntegral   _)    = GT
  compare (HsIsString _ _)    (HsFractional _)    = GT

instance Outputable OverLitVal where
  ppr (HsIntegral i)     = pprWithSourceText (il_text i) (integer (il_value i))
  ppr (HsFractional f)   = ppr f
  ppr (HsIsString st s)  = pprWithSourceText st (pprHsString s)

-- | @'hsLitNeedsParens' p l@ returns 'True' if a literal @l@ needs
-- to be parenthesized under precedence @p@.
hsLitNeedsParens :: PprPrec -> HsLit x -> Bool
hsLitNeedsParens p = go
  where
    go (HsChar {})        = False
    go (HsCharPrim {})    = False
    go (HsString {})      = False
    go (HsStringPrim {})  = False
    go (HsInt _ x)        = p > topPrec && il_neg x
    go (HsIntPrim _ x)    = p > topPrec && x < 0
    go (HsWordPrim {})    = False
    go (HsInt64Prim _ x)  = p > topPrec && x < 0
    go (HsWord64Prim {})  = False
    go (HsInteger _ x _)  = p > topPrec && x < 0
    go (HsRat _ x _)      = p > topPrec && fl_neg x
    go (HsFloatPrim _ x)  = p > topPrec && fl_neg x
    go (HsDoublePrim _ x) = p > topPrec && fl_neg x
    go (XLit _)           = False

-- | @'hsOverLitNeedsParens' p ol@ returns 'True' if an overloaded literal
-- @ol@ needs to be parenthesized under precedence @p@.
hsOverLitNeedsParens :: PprPrec -> HsOverLit x -> Bool
hsOverLitNeedsParens p (OverLit { ol_val = olv }) = go olv
  where
    go :: OverLitVal -> Bool
    go (HsIntegral x)   = p > topPrec && il_neg x
    go (HsFractional x) = p > topPrec && fl_neg x
    go (HsIsString {})  = False
hsOverLitNeedsParens _ (XOverLit { }) = False
