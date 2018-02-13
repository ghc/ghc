{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[HsLit]{Abstract syntax: source-language literals}
-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HsLit where

#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-} HsExpr( HsExpr, pprExpr )
import BasicTypes ( IntegralLit(..),FractionalLit(..),negateIntegralLit,
                    negateFractionalLit,SourceText(..),pprWithSourceText )
import Type       ( Type )
import Outputable
import FastString
import HsExtension

import Data.ByteString (ByteString)
import Data.Data hiding ( Fixity )

{-
************************************************************************
*                                                                      *
\subsection[HsLit]{Literals}
*                                                                      *
************************************************************************
-}

-- Note [Literal source text] in BasicTypes for SourceText fields in
-- the following
-- Note [Trees that grow] in HsExtension for the Xxxxx fields in the following
-- | Haskell Literal
data HsLit x
  = HsChar (XHsChar x) {- SourceText -} Char
      -- ^ Character
  | HsCharPrim (XHsCharPrim x) {- SourceText -} Char
      -- ^ Unboxed character
  | HsString (XHsString x) {- SourceText -} FastString
      -- ^ String
  | HsStringPrim (XHsStringPrim x) {- SourceText -} ByteString
      -- ^ Packed bytes
  | HsInt (XHsInt x)  IntegralLit
      -- ^ Genuinely an Int; arises from
      -- @TcGenDeriv@, and from TRANSLATION
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

deriving instance (DataId x) => Data (HsLit x)


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
        ol_val :: OverLitVal,
        ol_rebindable :: PostRn p Bool, -- Note [ol_rebindable]
        ol_witness :: HsExpr p,         -- Note [Overloaded literal witnesses]
        ol_type :: PostTc p Type }
deriving instance (DataId p) => Data (HsOverLit p)

-- Note [Literal source text] in BasicTypes for SourceText fields in
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

overLitType :: HsOverLit p -> PostTc p Type
overLitType = ol_type

-- | Convert a literal from one index type to another, updating the annotations
-- according to the relevant 'Convertable' instance
convertLit :: (ConvertIdX a b) => HsLit a -> HsLit b
convertLit (HsChar a x)       = (HsChar (convert a) x)
convertLit (HsCharPrim a x)   = (HsCharPrim (convert a) x)
convertLit (HsString a x)     = (HsString (convert a) x)
convertLit (HsStringPrim a x) = (HsStringPrim (convert a) x)
convertLit (HsInt a x)        = (HsInt (convert a) x)
convertLit (HsIntPrim a x)    = (HsIntPrim (convert a) x)
convertLit (HsWordPrim a x)   = (HsWordPrim (convert a) x)
convertLit (HsInt64Prim a x)  = (HsInt64Prim (convert a) x)
convertLit (HsWord64Prim a x) = (HsWord64Prim (convert a) x)
convertLit (HsInteger a x b)  = (HsInteger (convert a) x b)
convertLit (HsRat a x b)      = (HsRat (convert a) x b)
convertLit (HsFloatPrim a x)  = (HsFloatPrim (convert a) x)
convertLit (HsDoublePrim a x) = (HsDoublePrim (convert a) x)

{-
Note [ol_rebindable]
~~~~~~~~~~~~~~~~~~~~
The ol_rebindable field is True if this literal is actually
using rebindable syntax.  Specifically:

  False iff ol_witness is the standard one
  True  iff ol_witness is non-standard

Equivalently it's True if
  a) RebindableSyntax is on
  b) the witness for fromInteger/fromRational/fromString
     that happens to be in scope isn't the standard one

Note [Overloaded literal witnesses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Before* type checking, the HsExpr in an HsOverLit is the
name of the coercion function, 'fromInteger' or 'fromRational'.
*After* type checking, it is a witness for the literal, such as
        (fromInteger 3) or lit_78
This witness should replace the literal.

This dual role is unusual, because we're replacing 'fromInteger' with
a call to fromInteger.  Reason: it allows commoning up of the fromInteger
calls, which wouldn't be possible if the desugarer made the application.

The PostTcType in each branch records the type the overload literal is
found to have.
-}

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module MatchLit)
instance Eq (HsOverLit p) where
  (OverLit {ol_val = val1}) == (OverLit {ol_val=val2}) = val1 == val2

instance Eq OverLitVal where
  (HsIntegral   i1)   == (HsIntegral   i2)   = i1 == i2
  (HsFractional f1)   == (HsFractional f2)   = f1 == f2
  (HsIsString _ s1)   == (HsIsString _ s2)   = s1 == s2
  _                   == _                   = False

instance Ord (HsOverLit p) where
  compare (OverLit {ol_val=val1}) (OverLit {ol_val=val2}) = val1 `compare` val2

instance Ord OverLitVal where
  compare (HsIntegral i1)     (HsIntegral i2)     = i1 `compare` i2
  compare (HsIntegral _)      (HsFractional _)    = LT
  compare (HsIntegral _)      (HsIsString _ _)    = LT
  compare (HsFractional f1)   (HsFractional f2)   = f1 `compare` f2
  compare (HsFractional _)    (HsIntegral   _)    = GT
  compare (HsFractional _)    (HsIsString _ _)    = LT
  compare (HsIsString _ s1)   (HsIsString _ s2)   = s1 `compare` s2
  compare (HsIsString _ _)    (HsIntegral   _)    = GT
  compare (HsIsString _ _)    (HsFractional _)    = GT

-- Instance specific to GhcPs, need the SourceText
instance (SourceTextX x) => Outputable (HsLit x) where
    ppr (HsChar st c)       = pprWithSourceText (getSourceText st) (pprHsChar c)
    ppr (HsCharPrim st c)
     = pp_st_suffix (getSourceText st) primCharSuffix (pprPrimChar c)
    ppr (HsString st s)
      = pprWithSourceText (getSourceText st) (pprHsString s)
    ppr (HsStringPrim st s)
      = pprWithSourceText (getSourceText st) (pprHsBytes s)
    ppr (HsInt _ i)
      = pprWithSourceText (il_text i) (integer (il_value i))
    ppr (HsInteger st i _)  = pprWithSourceText (getSourceText st) (integer i)
    ppr (HsRat _ f _)       = ppr f
    ppr (HsFloatPrim _ f)   = ppr f <> primFloatSuffix
    ppr (HsDoublePrim _ d)  = ppr d <> primDoubleSuffix
    ppr (HsIntPrim st i)
      = pprWithSourceText (getSourceText st) (pprPrimInt i)
    ppr (HsWordPrim st w)
      = pprWithSourceText (getSourceText st) (pprPrimWord w)
    ppr (HsInt64Prim st i)
      = pp_st_suffix (getSourceText st) primInt64Suffix  (pprPrimInt64 i)
    ppr (HsWord64Prim st w)
      = pp_st_suffix (getSourceText st) primWord64Suffix (pprPrimWord64 w)

pp_st_suffix :: SourceText -> SDoc -> SDoc -> SDoc
pp_st_suffix NoSourceText         _ doc = doc
pp_st_suffix (SourceText st) suffix _   = text st <> suffix

-- in debug mode, print the expression that it's resolved to, too
instance (SourceTextX p, OutputableBndrId p)
       => Outputable (HsOverLit p) where
  ppr (OverLit {ol_val=val, ol_witness=witness})
        = ppr val <+> (whenPprDebug (parens (pprExpr witness)))

instance Outputable OverLitVal where
  ppr (HsIntegral i)     = pprWithSourceText (il_text i) (integer (il_value i))
  ppr (HsFractional f)   = ppr f
  ppr (HsIsString st s)  = pprWithSourceText st (pprHsString s)

-- | pmPprHsLit pretty prints literals and is used when pretty printing pattern
-- match warnings. All are printed the same (i.e., without hashes if they are
-- primitive and not wrapped in constructors if they are boxed). This happens
-- mainly for too reasons:
--  * We do not want to expose their internal representation
--  * The warnings become too messy
pmPprHsLit :: (SourceTextX x) => HsLit x -> SDoc
pmPprHsLit (HsChar _ c)       = pprHsChar c
pmPprHsLit (HsCharPrim _ c)   = pprHsChar c
pmPprHsLit (HsString st s)    = pprWithSourceText (getSourceText st)
                                                  (pprHsString s)
pmPprHsLit (HsStringPrim _ s) = pprHsBytes s
pmPprHsLit (HsInt _ i)        = integer (il_value i)
pmPprHsLit (HsIntPrim _ i)    = integer i
pmPprHsLit (HsWordPrim _ w)   = integer w
pmPprHsLit (HsInt64Prim _ i)  = integer i
pmPprHsLit (HsWord64Prim _ w) = integer w
pmPprHsLit (HsInteger _ i _)  = integer i
pmPprHsLit (HsRat _ f _)      = ppr f
pmPprHsLit (HsFloatPrim _ f)  = ppr f
pmPprHsLit (HsDoublePrim _ d) = ppr d

-- | Returns 'True' for compound literals that will need parentheses.
isCompoundHsLit :: HsLit x -> Bool
isCompoundHsLit (HsChar {})        = False
isCompoundHsLit (HsCharPrim {})    = False
isCompoundHsLit (HsString {})      = False
isCompoundHsLit (HsStringPrim {})  = False
isCompoundHsLit (HsInt _ x)        = il_neg x
isCompoundHsLit (HsIntPrim _ x)    = x < 0
isCompoundHsLit (HsWordPrim _ x)   = x < 0
isCompoundHsLit (HsInt64Prim _ x)  = x < 0
isCompoundHsLit (HsWord64Prim _ x) = x < 0
isCompoundHsLit (HsInteger _ x _)  = x < 0
isCompoundHsLit (HsRat _ x _)      = fl_neg x
isCompoundHsLit (HsFloatPrim _ x)  = fl_neg x
isCompoundHsLit (HsDoublePrim _ x) = fl_neg x

-- | Returns 'True' for compound overloaded literals that will need
-- parentheses when used in an argument position.
isCompoundHsOverLit :: HsOverLit x -> Bool
isCompoundHsOverLit (OverLit { ol_val = olv }) = compound_ol_val olv
  where
    compound_ol_val :: OverLitVal -> Bool
    compound_ol_val (HsIntegral x)   = il_neg x
    compound_ol_val (HsFractional x) = fl_neg x
    compound_ol_val (HsIsString {})  = False
