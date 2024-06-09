
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

import Language.Haskell.Syntax.Extension

import GHC.Types.SourceText (IntegralLit, FractionalLit, StringLit, SourceText, NoCommentsLocation)
import GHC.Core.Type (Type)
import GHC.Utils.Panic (panic)

import GHC.Data.FastString (FastString)

import Data.ByteString (ByteString)
import Data.Data hiding ( Fixity )
import Data.Bool
import Data.Ord
import Data.Eq
import Data.Char
import Prelude (Maybe, Integer)

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
  | HsInt (XHsInt x)  (IntegralLit x)
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
  | HsInteger (XHsInteger x) {- SourceText -} Integer Type
      -- ^ Genuinely an integer; arises only
      -- from TRANSLATION (overloaded
      -- literals are done with HsOverLit)
  | HsRat (XHsRat x)  (FractionalLit x) Type
      -- ^ Genuinely a rational; arises only from
      -- TRANSLATION (overloaded literals are
      -- done with HsOverLit)
  | HsFloatPrim (XHsFloatPrim x)   (FractionalLit x)
      -- ^ Unboxed Float
  | HsDoublePrim (XHsDoublePrim x) (FractionalLit x)
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
data HsOverLit pass
  = OverLit {
      ol_ext :: (XOverLit pass),
      ol_val :: OverLitVal pass}

  | XOverLit
      !(XXOverLit pass)

-- Note [Literal source text] in "GHC.Types.SourceText" for SourceText fields in
-- the following
-- | Overloaded Literal Value
data OverLitVal pass
  = HsIntegral   !(IntegralLit pass)   -- ^ Integer-looking literals;
  | HsFractional !(FractionalLit pass) -- ^ Frac-looking literals
  | HsIsString   !(StringLit pass)     -- ^ String-looking literals

deriving instance (Data pass, XIntegralLit pass ~ SourceText, XFractionalLit pass ~ SourceText, XStringLit pass ~ (SourceText, Maybe NoCommentsLocation)) => Data (OverLitVal pass)

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module GHC.HsToCore.Match.Literal)
instance (Eq (XXOverLit pass)) => Eq (HsOverLit pass) where
  (OverLit _ val1) == (OverLit _ val2) = val1 == val2
  (XOverLit  val1) == (XOverLit  val2) = val1 == val2
  _ == _ = panic "Eq HsOverLit"

instance Eq (OverLitVal pass) where
  (HsIntegral   i1)   == (HsIntegral   i2)   = i1 == i2
  (HsFractional f1)   == (HsFractional f2)   = f1 == f2
  (HsIsString   s1)   == (HsIsString   s2)   = s1 == s2
  _                   == _                   = False

instance (Ord (XXOverLit pass)) => Ord (HsOverLit pass) where
  compare (OverLit _ val1)  (OverLit _ val2) = val1 `compare` val2
  compare (XOverLit  val1)  (XOverLit  val2) = val1 `compare` val2
  compare _ _ = panic "Ord HsOverLit"

instance Ord (OverLitVal pass) where
  compare (HsIntegral i1)     (HsIntegral i2)     = i1 `compare` i2
  compare (HsIntegral _)      (HsFractional _)    = LT
  compare (HsIntegral _)      (HsIsString   _)    = LT
  compare (HsFractional f1)   (HsFractional f2)   = f1 `compare` f2
  compare (HsFractional _)    (HsIntegral   _)    = GT
  compare (HsFractional _)    (HsIsString   _)    = LT
  compare (HsIsString   s1)   (HsIsString   s2)   = s1 `compare` s2
  compare (HsIsString   _)    (HsIntegral   _)    = GT
  compare (HsIsString   _)    (HsFractional _)    = GT
