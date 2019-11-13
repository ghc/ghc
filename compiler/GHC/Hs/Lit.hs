{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[HsLit]{Abstract syntax: source-language literals}
-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module GHC.Hs.Extension
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Hs.Lit where

#include "HsVersions.h"

import GHC.Prelude

import {-# SOURCE #-} GHC.Hs.Expr( HsExpr, pprExpr )
import GHC.Types.Basic
   ( IntegralLit(..), FractionalLit(..), negateIntegralLit
   , negateFractionalLit, SourceText(..), pprWithSourceText
   , PprPrec(..), topPrec )
import GHC.Core.Type
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Hs.Extension

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
-- Note [Trees that grow] in GHC.Hs.Extension for the Xxxxx fields in the following
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
      -- @GHC.Tc.Deriv.Generate@, and from TRANSLATION
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

type instance XHsChar       (GhcPass _) = SourceText
type instance XHsCharPrim   (GhcPass _) = SourceText
type instance XHsString     (GhcPass _) = SourceText
type instance XHsStringPrim (GhcPass _) = SourceText
type instance XHsInt        (GhcPass _) = NoExtField
type instance XHsIntPrim    (GhcPass _) = SourceText
type instance XHsWordPrim   (GhcPass _) = SourceText
type instance XHsInt64Prim  (GhcPass _) = SourceText
type instance XHsWord64Prim (GhcPass _) = SourceText
type instance XHsInteger    (GhcPass _) = SourceText
type instance XHsRat        (GhcPass _) = NoExtField
type instance XHsFloatPrim  (GhcPass _) = NoExtField
type instance XHsDoublePrim (GhcPass _) = NoExtField
type instance XXLit         (GhcPass _) = NoExtCon

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
      ol_val :: OverLitVal,
      ol_witness :: HsExpr p}         -- Note [Overloaded literal witnesses]

  | XOverLit
      !(XXOverLit p)

data OverLitTc
  = OverLitTc {
        ol_rebindable :: Bool, -- Note [ol_rebindable]
        ol_type :: Type }
  deriving Data

type instance XOverLit GhcPs = NoExtField
type instance XOverLit GhcRn = Bool            -- Note [ol_rebindable]
type instance XOverLit GhcTc = OverLitTc

type instance XXOverLit (GhcPass _) = NoExtCon

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

overLitType :: HsOverLit GhcTc -> Type
overLitType (OverLit (OverLitTc _ ty) _ _) = ty

-- | Convert a literal from one index type to another
convertLit :: HsLit (GhcPass p1) -> HsLit (GhcPass p2)
convertLit (HsChar a x)       = HsChar a x
convertLit (HsCharPrim a x)   = HsCharPrim a x
convertLit (HsString a x)     = HsString a x
convertLit (HsStringPrim a x) = HsStringPrim a x
convertLit (HsInt a x)        = HsInt a x
convertLit (HsIntPrim a x)    = HsIntPrim a x
convertLit (HsWordPrim a x)   = HsWordPrim a x
convertLit (HsInt64Prim a x)  = HsInt64Prim a x
convertLit (HsWord64Prim a x) = HsWord64Prim a x
convertLit (HsInteger a x b)  = HsInteger a x b
convertLit (HsRat a x b)      = HsRat a x b
convertLit (HsFloatPrim a x)  = HsFloatPrim a x
convertLit (HsDoublePrim a x) = HsDoublePrim a x

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
-- for compiling pattern-matching (module GHC.HsToCore.Match.Literal)
instance (Eq (XXOverLit p)) => Eq (HsOverLit p) where
  (OverLit _ val1 _) == (OverLit _ val2 _) = val1 == val2
  (XOverLit  val1)   == (XOverLit  val2)   = val1 == val2
  _ == _ = panic "Eq HsOverLit"

instance Eq OverLitVal where
  (HsIntegral   i1)   == (HsIntegral   i2)   = i1 == i2
  (HsFractional f1)   == (HsFractional f2)   = f1 == f2
  (HsIsString _ s1)   == (HsIsString _ s2)   = s1 == s2
  _                   == _                   = False

instance (Ord (XXOverLit p)) => Ord (HsOverLit p) where
  compare (OverLit _ val1 _) (OverLit _ val2 _) = val1 `compare` val2
  compare (XOverLit  val1)   (XOverLit  val2)   = val1 `compare` val2
  compare _ _ = panic "Ord HsOverLit"

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
instance Outputable (HsLit (GhcPass p)) where
    ppr (HsChar st c)       = pprWithSourceText st (pprHsChar c)
    ppr (HsCharPrim st c)   = pp_st_suffix st primCharSuffix (pprPrimChar c)
    ppr (HsString st s)     = pprWithSourceText st (pprHsString s)
    ppr (HsStringPrim st s) = pprWithSourceText st (pprHsBytes s)
    ppr (HsInt _ i)
      = pprWithSourceText (il_text i) (integer (il_value i))
    ppr (HsInteger st i _)  = pprWithSourceText st (integer i)
    ppr (HsRat _ f _)       = ppr f
    ppr (HsFloatPrim _ f)   = ppr f <> primFloatSuffix
    ppr (HsDoublePrim _ d)  = ppr d <> primDoubleSuffix
    ppr (HsIntPrim st i)    = pprWithSourceText st (pprPrimInt i)
    ppr (HsWordPrim st w)   = pprWithSourceText st (pprPrimWord w)
    ppr (HsInt64Prim st i)  = pp_st_suffix st primInt64Suffix  (pprPrimInt64 i)
    ppr (HsWord64Prim st w) = pp_st_suffix st primWord64Suffix (pprPrimWord64 w)

pp_st_suffix :: SourceText -> SDoc -> SDoc -> SDoc
pp_st_suffix NoSourceText         _ doc = doc
pp_st_suffix (SourceText st) suffix _   = text st <> suffix

-- in debug mode, print the expression that it's resolved to, too
instance OutputableBndrId p
       => Outputable (HsOverLit (GhcPass p)) where
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
pmPprHsLit :: HsLit (GhcPass x) -> SDoc
pmPprHsLit (HsChar _ c)       = pprHsChar c
pmPprHsLit (HsCharPrim _ c)   = pprHsChar c
pmPprHsLit (HsString st s)    = pprWithSourceText st (pprHsString s)
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
