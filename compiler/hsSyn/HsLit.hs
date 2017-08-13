{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[HsLit]{Abstract syntax: source-language literals}
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module HsLit
  ( HsLit
      , pattern HsChar
      , pattern HsCharPrim
      , pattern HsString
      , pattern HsStringPrim
      , pattern HsInt
      , pattern HsIntPrim
      , pattern HsWordPrim
      , pattern HsInt64Prim
      , pattern HsWord64Prim
      , pattern HsInteger
      , pattern HsRat
      , pattern HsFloatPrim
      , pattern HsDoublePrim
  , HsOverLit
      , pattern OverLit
          , ol_val
          , ol_rebindable
          , ol_witness
          , ol_type
  , LHsOverLit
  , OverLitVal
      , pattern HsIntegral
      , pattern HsFractional
      , pattern HsIsString
  , negateOverLitVal
  , overLitType
  , convertLit
  , pmPprHsLit
  ) where

#include "HsVersions.h"

import {-# SOURCE #-} HsExpr( HsExpr, pprExpr )
import BasicTypes ( IntegralLit(..),FractionalLit(..),negateIntegralLit,
                    negateFractionalLit,SourceText(..),pprWithSourceText )
import Type       ( Type )
import Outputable
import FastString
import HsExtension

import Data.ByteString (ByteString)
import Data.Data hiding ( Fixity )

import qualified AST

-- -----------------------------------------------------------------------------
-- * Data Declarations
-- -----------------------------------------------------------------------------

-- Note [Literal source text] in BasicTypes for SourceText fields in
-- the following
-- Note [Trees that grow] in HsExtension for the Xxxxx fields in the following

-- | Haskell Literal
type HsLit x = AST.Lit (GHC x)

pattern
   HsChar :: (XHsChar x) -> Char -> HsLit x
   -- ^ Character
pattern
   HsCharPrim :: (XHsCharPrim x) -> Char -> HsLit x
   -- ^ Unboxed character
pattern
   HsString :: (XHsString x) -> FastString -> HsLit x
   -- ^ String
pattern
   HsStringPrim :: (XHsStringPrim x) -> ByteString -> HsLit x
   -- ^ Packed bytes
pattern
   HsInt :: (XHsInt x) -> IntegralLit -> HsLit x
   -- ^ Genuinely an Int; arises from
   -- @TcGenDeriv@, and from TRANSLATION
pattern
   HsIntPrim :: (XHsIntPrim x) -> Integer -> HsLit x
   -- ^ literal @Int#@
pattern
   HsWordPrim :: (XHsWordPrim x) -> Integer -> HsLit x
   -- ^ literal @Word#@
pattern
   HsInt64Prim :: (XHsInt64Prim x) -> Integer -> HsLit x
   -- ^ literal @Int64#@
pattern
   HsWord64Prim :: (XHsWord64Prim x) -> Integer -> HsLit x
   -- ^ literal @Word64#@
pattern
   HsInteger :: (XHsInteger x) -> Integer -> Type -> HsLit x
   -- ^ Genuinely an integer; arises only
   -- from TRANSLATION (overloaded
   -- literals are done with HsOverLit)
pattern
   HsRat :: (XHsRat x) -> FractionalLit -> Type -> HsLit x
   -- ^ Genuinely a rational; arises only from
   -- TRANSLATION (overloaded literals are
   -- done with HsOverLit)
pattern
   HsFloatPrim :: (XHsFloatPrim x) -> FractionalLit -> HsLit x
   -- ^ Unboxed Float
pattern
   HsDoublePrim :: (XHsDoublePrim x) -> FractionalLit -> HsLit x
   -- ^ Unboxed Double

pattern
   HsChar a b = AST.Char a b
pattern
   HsCharPrim a b = AST.CharPrim a b
pattern
   HsString a b = AST.String a b
pattern
   HsStringPrim a b = AST.StringPrim a b
pattern
   HsInt a b = AST.NewLit (NHsInt a b)
pattern
   HsIntPrim a b = AST.IntPrim a b
pattern
   HsWordPrim a b = AST.WordPrim a b
pattern
   HsInt64Prim a b = AST.Int64Prim a b
pattern
   HsWord64Prim a b = AST.Word64Prim a b
pattern
   HsInteger a b c = AST.NewLit (NHsInteger a b c)
pattern
   HsRat a b c = AST.NewLit (NHsRat a b c)
pattern
   HsFloatPrim a b = AST.FloatPrim a b
pattern
   HsDoublePrim a b = AST.DoublePrim a b

{-# COMPLETE
      HsChar,
      HsCharPrim,
      HsString,
      HsStringPrim,
      HsInt,
      HsIntPrim,
      HsWordPrim,
      HsInt64Prim,
      HsWord64Prim,
      HsInteger,
      HsRat,
      HsFloatPrim,
      HsDoublePrim
  #-}

type instance
   AST.XChar       (GHC pass) = XHsChar       pass
type instance
   AST.XCharPrim   (GHC pass) = XHsCharPrim   pass
type instance
   AST.XString     (GHC pass) = XHsString     pass
type instance
   AST.XStringPrim (GHC pass) = XHsStringPrim pass
type instance
   AST.XIntPrim    (GHC pass) = XHsIntPrim    pass
type instance
   AST.XWordPrim   (GHC pass) = XHsWordPrim   pass
type instance
   AST.XInt64Prim  (GHC pass) = XHsInt64Prim  pass
type instance
   AST.XWord64Prim (GHC pass) = XHsWord64Prim pass
type instance
   AST.XFloatPrim  (GHC pass) = XHsFloatPrim  pass
type instance
   AST.XDoublePrim (GHC pass) = XHsDoublePrim pass
type instance
   AST.XNewLit     (GHC pass) = NewHsLit      pass

data NewHsLit pass
  = NHsInt
      (XHsInt pass)
      IntegralLit

  | NHsInteger
      (XHsInteger pass)
      Integer
      Type

  | NHsRat
      (XHsRat pass)
      FractionalLit
      Type

-- ------------------------------------

type
  HsOverLit pass = AST.OverLit (GHC pass)
-- ^ Haskell Overloaded Literal
pattern
  OverLit ::
    (OverLitVal) ->
    (PostRn pass Bool) -> -- Note [ol_rebindable]
    (HsExpr pass) ->      -- Note [Overloaded literal witnesses]
    (PostTc pass Type) ->
    (HsOverLit pass)
pattern
  OverLit { ol_val, ol_rebindable, ol_witness, ol_type }
    = AST.OverLit (ol_rebindable, ol_witness, ol_type) ol_val

{-#
  COMPLETE
    OverLit
  #-}

type instance
  AST.XOverLit    (GHC pass) = ( PostRn pass Bool
                               , HsExpr pass
                               , PostTc pass Type)
type instance
  AST.XNewOverLit (GHC pass) = NoConExt

type LHsOverLit pass = AST.LOverLit (GHC pass)

-- ------------------------------------

type
  OverLitVal = AST.OverLitVal
pattern
  HsIntegral ::
    IntegralLit ->
    OverLitVal
  -- ^ Integer-looking literals;
pattern
  HsFractional ::
    FractionalLit ->
    OverLitVal
  -- ^ Frac-looking literals
pattern
  HsIsString ::
    SourceText ->
    FastString ->
    OverLitVal
  -- ^ String-looking literals

pattern
  HsIntegral a
    = AST.Integral a

pattern
  HsFractional a
    = AST.Fractional a

pattern
  HsIsString a b
    = AST.IsString a b

{-#
  COMPLETE
    HsIntegral,
    HsFractional,
    HsIsString
  #-}

-- -----------------------------------------------------------------------------
-- * Utilities
-- -----------------------------------------------------------------------------

deriving instance
  (DataId pass) => Data (NewHsLit pass)

deriving instance
  (DataId x) => Data (HsLit x)

deriving instance
  (DataId p) => Data (HsOverLit p)

deriving instance
  Data OverLitVal

-- ------------------------------------

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

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module MatchLit)
instance Eq (HsOverLit p) where
  (OverLit {ol_val = val1}) == (OverLit {ol_val=val2}) = val1 == val2

instance Eq OverLitVal where
  (HsIntegral   i1)   == (HsIntegral   i2)   = i1 == i2
  (HsFractional f1)   == (HsFractional f2)   = f1 == f2
  (HsIsString _ s1)   == (HsIsString _ s2)   = s1 == s2
  _                   == _                   = False

-- ------------------------------------

negateOverLitVal :: OverLitVal  -> OverLitVal
negateOverLitVal (HsIntegral i)   = HsIntegral (negateIntegralLit i)
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

-- -----------------------------------------------------------------------------
-- * Pretty Printing
-- -----------------------------------------------------------------------------

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
        = ppr val <+> (ifPprDebug (parens (pprExpr witness)))

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

-- -----------------------------------------------------------------------------
-- Notes
-- -----------------------------------------------------------------------------

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
