
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

import Prelude
import Language.Haskell.Syntax.Extension

import GHC.Core.Type (Type)

import GHC.Data.FastString (FastString, lexicalCompareFS)

import Data.ByteString (ByteString)
import Data.Data hiding ( Fixity )
import Data.Function (on)

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

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module GHC.HsToCore.Match.Literal)
instance (Eq (XXOverLit pass)) => Eq (HsOverLit pass) where
  (OverLit _ val1) == (OverLit _ val2) = val1 == val2
  (XOverLit  val1) == (XOverLit  val2) = val1 == val2
  _ == _ = False

instance Eq (OverLitVal pass) where
  (HsIntegral   i1)   == (HsIntegral   i2)   = i1 == i2
  (HsFractional f1)   == (HsFractional f2)   = f1 == f2
  (HsIsString   s1)   == (HsIsString   s2)   = s1 == s2
  _                   == _                   = False

instance (Ord (XXOverLit pass)) => Ord (HsOverLit pass) where
  compare (OverLit _ val1)  (OverLit _ val2) = val1 `compare` val2
  compare (XOverLit  val1)  (XOverLit  val2) = val1 `compare` val2
  compare (OverLit _ _)     (XOverLit _)     = GT
  compare (XOverLit _)     (OverLit _ _)     = LT

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

------------------------------------------------
-- Literals
------------------------------------------------

-- | Integral Literal
--
-- Used (instead of Integer) to represent negative zegative zero which is
-- required for NegativeLiterals extension to correctly parse `-0::Double`
-- as negative zero. See also #13211.
data IntegralLit pass = IL
   { il_text  :: XIntegralLit pass
   , il_neg   :: Bool -- See Note [Negative zero] in GHC.Rename.Pat
   , il_value :: Integer
   }

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module GHC.HsToCore.Match.Literal)

instance Eq (IntegralLit pass) where
  (==) = (==) `on` il_value

instance Ord (IntegralLit pass) where
  compare = compare `on` il_value

-- | The Show instance is required for the derived GHC.Parser.Lexer.Token instance when DEBUG is on
deriving instance Show (XIntegralLit pass) => Show (IntegralLit pass)

-- | Fractional Literal
--
-- Used (instead of Rational) to represent exactly the floating point literal that we
-- encountered in the user's source program. This allows us to pretty-print exactly what
-- the user wrote, which is important e.g. for floating point numbers that can't represented
-- as Doubles (we used to via Double for pretty-printing). See also #2245.
-- Note [FractionalLit representation] in GHC.HsToCore.Match.Literal
-- The actual value then is: sign * fl_signi * (fl_exp_base^fl_exp)
--                             where sign = if fl_neg then (-1) else 1
--
-- For example FL { fl_neg = True, fl_signi = 5.3, fl_exp = 4, fl_exp_base = Base10 }
-- denotes  -5300

data FractionalLit pass = FL
    { fl_text :: XFractionalLit pass       -- ^ How the value was written in the source
    , fl_neg :: Bool                        -- See Note [Negative zero]
    , fl_signi :: Rational                  -- The significand component of the literal
    , fl_exp :: Integer                     -- The exponent component of the literal
    , fl_exp_base :: FractionalExponentBase -- See Note [fractional exponent bases]
    }

-- See Note [FractionalLit representation] in GHC.HsToCore.Match.Literal
data FractionalExponentBase
  = Base2 -- Used in hex fractional literals
  | Base10
  deriving (Eq, Ord, Data, Show)

-- TODO
{- Note [fractional exponent bases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For hexadecimal rationals of
the form 0x0.3p10 the exponent is given on base 2 rather than
base 10. These are the only options, hence the sum type. See also #15646.
-}

-- | Be wary of using this instance to compare for equal *values* when exponents are
-- large. The same value expressed in different syntactic form won't compare as equal when
-- any of the exponents is >= 100.
instance Eq (FractionalLit pass) where
  (==) fl1 fl2 = case compare fl1 fl2 of
          EQ -> True
          _  -> False

-- | Be wary of using this instance to compare for equal *values* when exponents are
-- large. The same value expressed in different syntactic form won't compare as equal when
-- any of the exponents is >= 100.
instance Ord (FractionalLit pass) where
  compare = compareFractionalLit

-- | The Show instance is required for the derived GHC.Parser.Lexer.Token instance when DEBUG is on
deriving instance Show (XFractionalLit pass) => Show (FractionalLit pass)

-- | Compare fractional lits with small exponents for value equality but
--   large values for syntactic equality.
compareFractionalLit :: FractionalLit pass -> FractionalLit pass -> Ordering
compareFractionalLit fl1 fl2
  | fl_exp fl1 < 100 && fl_exp fl2 < 100 && fl_exp fl1 >= -100 && fl_exp fl2 >= -100
    = rationalFromFractionalLit fl1 `compare` rationalFromFractionalLit fl2
  | otherwise = (compare `on` (\x -> (fl_signi x, fl_exp x, fl_exp_base x))) fl1 fl2

rationalFromFractionalLit :: FractionalLit pass -> Rational
rationalFromFractionalLit (FL _ _ i e expBase) =
  mkRationalWithExponentBase i e expBase

mkRationalWithExponentBase :: Rational -> Integer -> FractionalExponentBase -> Rational
mkRationalWithExponentBase i e feb = i * (eb ^^ e)
  where eb = case feb of Base2 -> 2 ; Base10 -> 10

-- | A String Literal in the source, including its original raw format for use by
-- source to source manipulation tools.
data StringLit pass = SL
  { sl_st :: XStringLit pass, -- literal raw source.
                         -- See Note [Literal source text]
    sl_fs :: FastString  -- literal string value
  }

instance Eq (StringLit pass) where
  (SL _ a) == (SL _ b) = a == b

instance Ord (StringLit pass) where
  (SL _ a) `compare` (SL _ b) = a `lexicalCompareFS` b

-- | The Show instance is required for the derived GHC.Parser.Lexer.Token instance when DEBUG is on
deriving instance Show (XStringLit pass) => Show (StringLit pass)
