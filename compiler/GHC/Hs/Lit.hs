{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable, OutputableBndrId

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Source-language literals
module GHC.Hs.Lit (
  -- * Literal Syntax
  -- ** Normal Haskell Literal
  -- *** Data-type
    HsLit(..)
  -- *** Queries
  , hsLitNeedsParens
  -- *** Conversion
  , convertLit
  -- *** Typechecked Literal
  , HsLitTc(..)

  -- ** Overloaded Haskell Literal Types
  -- *** HsOverLit
  , HsOverLit(..)
  , hsOverLitNeedsParens
  , overLitType
  -- *** OverLitVal
  , OverLitVal(..)
  , negateOverLitVal
  -- *** Conversion
  , rnOverLitVal
  , tcOverLitVal
  -- *** Pass specific records
  , OverLitRn(..)
  , OverLitTc(..)

  -- ** Haskell Qualified Literal
  , HsQualLit(..)
  , QualLitVal(..)

  -- * Literal Values
  -- ** Fractional
  -- *** Data-types
  , FractionalLit(..)
  , FractionalExponentBase(..)
  -- *** Construction
  , mkFractionalLit
  , mkFractionalLitFromText
  , mkFractionalLitFromRational
  , mkFractionalLitFromInteger
  , mkTHFractionalLit
  -- *** Conversion
  , rationalFromFractionalLit
  , negateFractionalLit
  , rnFractionalLit
  , tcFractionalLit

  -- ** Integral
  -- *** Data-type
  , IntegralLit(..)
  -- *** Construction
  , mkIntegralLit
  -- *** Conversion
  , negateIntegralLit
  , rnIntegralLit
  , tcIntegralLit

  -- ** Textual
  -- *** Data-type
  , StringLiteral(..)
  -- *** Conversion
  , pprHsStringLit
  , rnStringLit
  , tcStringLit
  -- *** Query
  , stringLitSourceText

  -- * Deprecated Functions
  , fractionalLitFromRational
  , integralFractionalLit
  , mkSourceFractionalLit
  ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Hs.Expr( pprExpr )

import GHC.Data.FastString
import GHC.Types.Basic (PprPrec(..), topPrec )
import GHC.Core.Ppr ( {- instance OutputableBndr TyVar -} )
import GHC.Types.SourceText
import GHC.Core.Type
import GHC.Utils.Misc (split)
import GHC.Utils.Outputable
import GHC.Utils.Panic (panic)
import GHC.Hs.Extension

import Language.Haskell.Syntax.Expr ( HsExpr )
import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Lit
import Language.Haskell.Syntax.Module.Name (moduleNameString)

import Data.Function (on)
import Data.Ratio ((%))

{-
************************************************************************
*                                                                      *
\subsection[HsLit]{Literals}
*                                                                      *
************************************************************************
-}

-- Note [Literal source text] in "GHC.Types.SourceText" for the extension of a
-- SourceText field in all of the following type family instances for 'HsLit'.
type instance XHsChar       (GhcPass _) = SourceText
type instance XHsCharPrim   (GhcPass _) = SourceText
type instance XHsString     (GhcPass _) = SourceText
type instance XHsStringPrim (GhcPass _) = SourceText
type instance XHsNatural    (GhcPass _) = NoExtField
type instance XHsDouble     (GhcPass _) = NoExtField
type instance XHsInt        (GhcPass _) = NoExtField
type instance XHsIntPrim    (GhcPass _) = SourceText
type instance XHsWordPrim   (GhcPass _) = SourceText
type instance XHsInt8Prim   (GhcPass _) = SourceText
type instance XHsInt16Prim  (GhcPass _) = SourceText
type instance XHsInt32Prim  (GhcPass _) = SourceText
type instance XHsInt64Prim  (GhcPass _) = SourceText
type instance XHsWord8Prim  (GhcPass _) = SourceText
type instance XHsWord16Prim (GhcPass _) = SourceText
type instance XHsWord32Prim (GhcPass _) = SourceText
type instance XHsWord64Prim (GhcPass _) = SourceText
type instance XHsFloatPrim  (GhcPass _) = NoExtField
type instance XHsDoublePrim (GhcPass _) = NoExtField

type instance XXLit         GhcPs = DataConCantHappen
type instance XXLit         GhcRn = DataConCantHappen
type instance XXLit         GhcTc = HsLitTc

data HsLitTc
  = HsInteger SourceText Integer Type
      -- ^ Genuinely an integer; arises only
      -- from TRANSLATION (overloaded
      -- literals are done with HsOverLit)
  | HsRat (FractionalLit GhcTc) Type
      -- ^ Genuinely a rational; arises only from
      -- TRANSLATION (overloaded literals are
      -- done with HsOverLit)
instance Eq HsLitTc where
  (HsInteger _ x _) == (HsInteger _ y _) = x==y
  (HsRat x _)       == (HsRat y _)       = x==y
  _                 == _                 = False

data OverLitRn
  = OverLitRn {
        ol_rebindable :: Bool,         -- Note [ol_rebindable]
        ol_from_fun   :: LIdP GhcRn    -- Note [Overloaded literal witnesses]
        }

data OverLitTc
  = OverLitTc {
        ol_rebindable :: Bool,         -- Note [ol_rebindable]
        ol_witness    :: HsExpr GhcTc, -- Note [Overloaded literal witnesses]
        ol_type :: Type }

{-
Note [Overloaded literal witnesses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

During renaming, the coercion function needed for a given HsOverLit is
resolved according to the current scope and RebindableSyntax (see Note
[ol_rebindable]). The result of this resolution *before* type checking
is the coercion function such as 'fromInteger' or 'fromRational',
stored in the ol_from_fun field of OverLitRn.

*After* type checking, the ol_witness field of the OverLitTc contains
the witness of the literal as HsExpr, such as (fromInteger 3) or
lit_78. This witness should replace the literal. Reason: it allows
commoning up of the fromInteger calls, which wouldn't be possible if
the desugarer made the application.

The ol_type in OverLitTc records the type the overloaded literal is
found to have.
-}

type instance XOverLit GhcPs = NoExtField
type instance XOverLit GhcRn = OverLitRn
type instance XOverLit GhcTc = OverLitTc

pprXOverLit :: GhcPass p -> XOverLit (GhcPass p) -> SDoc
pprXOverLit GhcPs noExt = ppr noExt
pprXOverLit GhcRn OverLitRn{ ol_from_fun = from_fun } = ppr from_fun
pprXOverLit GhcTc OverLitTc{ ol_witness = witness } = pprExpr witness

type instance XXOverLit (GhcPass _) = DataConCantHappen

overLitType :: HsOverLit GhcTc -> Type
overLitType (OverLit OverLitTc{ ol_type = ty } _) = ty

-- | @'hsOverLitNeedsParens' p ol@ returns 'True' if an overloaded literal
-- @ol@ needs to be parenthesized under precedence @p@.
hsOverLitNeedsParens :: PprPrec -> HsOverLit (GhcPass p) -> Bool
hsOverLitNeedsParens p (OverLit { ol_val = olv }) = go olv
  where
    go :: OverLitVal (GhcPass p) -> Bool
    go (HsIntegral x)   = p > topPrec && il_neg x
    go (HsFractional x) = p > topPrec && fl_neg x
    go (HsIsString {})  = False

-- | @'hsLitNeedsParens' p l@ returns 'True' if a literal @l@ needs
-- to be parenthesized under precedence @p@.
--
-- See Note [Printing of literals in Core] in GHC.Types.Literal
-- for the reasoning.
hsLitNeedsParens :: forall x. IsPass x => PprPrec -> HsLit (GhcPass x) -> Bool
hsLitNeedsParens p = go
  where
    go (HsChar {})        = False
    go (HsCharPrim {})    = False
    go (HsString {})      = False
    go (HsStringPrim {})  = False
    go (HsNatural _ x)    = p > topPrec && il_neg x
    go (HsDouble  _ x)    = p > topPrec && fl_neg x
    go (HsInt _ x)        = p > topPrec && il_neg x
    go (HsFloatPrim {})   = False
    go (HsDoublePrim {})  = False
    go (HsIntPrim {})     = False
    go (HsInt8Prim {})    = False
    go (HsInt16Prim {})   = False
    go (HsInt32Prim {})   = False
    go (HsInt64Prim {})   = False
    go (HsWordPrim {})    = False
    go (HsWord8Prim {})   = False
    go (HsWord16Prim {})  = False
    go (HsWord64Prim {})  = False
    go (HsWord32Prim {})  = False
    go (XLit x)           = case ghcPass @x of
      GhcTc -> case x of
         (HsInteger _ x _) -> p > topPrec && x < 0
         (HsRat  x _)      -> p > topPrec && fl_neg x

-- | Convert a literal from one index type to another.
-- The constraint XXLit (GhcPass p)~DataConCantHappen means that once the
-- XLit constructor is inhabited, we can no longer go back to the case where
-- its not. In practice it just means you can't just convertLit to go from
-- (HsLit GhcTc) -> (HsLit GhcPs/GhcRn), while all other conversions are fine.
convertLit :: XXLit (GhcPass p)~DataConCantHappen => HsLit (GhcPass p) -> HsLit (GhcPass p')
convertLit (HsChar a x)       = HsChar a x
convertLit (HsCharPrim a x)   = HsCharPrim a x
convertLit (HsString a x)     = HsString a x
convertLit (HsStringPrim a x) = HsStringPrim a x
convertLit (HsIntPrim a x)    = HsIntPrim a x
convertLit (HsWordPrim a x)   = HsWordPrim a x
convertLit (HsInt8Prim a x)   = HsInt8Prim a x
convertLit (HsInt16Prim a x)  = HsInt16Prim a x
convertLit (HsInt32Prim a x)  = HsInt32Prim a x
convertLit (HsInt64Prim a x)  = HsInt64Prim a x
convertLit (HsWord8Prim a x)  = HsWord8Prim a x
convertLit (HsWord16Prim a x) = HsWord16Prim a x
convertLit (HsWord32Prim a x) = HsWord32Prim a x
convertLit (HsWord64Prim a x) = HsWord64Prim a x
convertLit (HsDouble     a x) = HsDouble     a $ convertFractionalLit x
convertLit (HsDoublePrim a x) = HsDoublePrim a $ convertFractionalLit x
convertLit (HsFloatPrim  a x) = HsFloatPrim  a $ convertFractionalLit x
convertLit (HsInt        a x) = HsInt        a $ convertIntegralLit x
convertLit (HsNatural    a x) = HsNatural    a $ convertIntegralLit x

{-
Note [ol_rebindable]
~~~~~~~~~~~~~~~~~~~~
The ol_rebindable field is True if this literal is actually
using rebindable syntax.  Specifically:

  False iff ol_from_fun / ol_witness is the standard one
  True  iff ol_from_fun / ol_witness is non-standard

Equivalently it's True if
  a) RebindableSyntax is on
  b) the witness for fromInteger/fromRational/fromString
     that happens to be in scope isn't the standard one
-}

-- -----------------------------------------------------------------------------
-- HsLit

instance Eq (XXLit (GhcPass p)) => Eq (HsLit (GhcPass p)) where
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

-- Instance specific to GhcPs, need the SourceText
instance IsPass p => Outputable (HsLit (GhcPass p)) where
    ppr (HsChar st c)       = pprWithSourceText st (pprHsChar c)
    ppr (HsCharPrim st c)   = pprWithSourceText st (pprPrimChar c)
    ppr (HsString st s)     = pprHsStringLit st s
    ppr (HsStringPrim st s) = pprWithSourceText st (pprHsBytes s)
    ppr (HsDouble _ d)      = ppr d
    ppr (HsNatural _ i)     = pprIntegralLit i
    ppr (HsInt _ i)         = pprIntegralLit i
    ppr (HsFloatPrim _ f)   = ppr f <> primFloatSuffix
    ppr (HsDoublePrim _ d)  = ppr d <> primDoubleSuffix
    ppr (HsIntPrim st i)    = pprWithSourceText st (pprPrimInt i)
    ppr (HsInt8Prim st i)   = pprWithSourceText st (pprPrimInt8 i)
    ppr (HsInt16Prim st i)  = pprWithSourceText st (pprPrimInt16 i)
    ppr (HsInt32Prim st i)  = pprWithSourceText st (pprPrimInt32 i)
    ppr (HsInt64Prim st i)  = pprWithSourceText st (pprPrimInt64 i)
    ppr (HsWordPrim st w)   = pprWithSourceText st (pprPrimWord w)
    ppr (HsWord8Prim st w)  = pprWithSourceText st (pprPrimWord8 w)
    ppr (HsWord16Prim st w) = pprWithSourceText st (pprPrimWord16 w)
    ppr (HsWord32Prim st w) = pprWithSourceText st (pprPrimWord32 w)
    ppr (HsWord64Prim st w) = pprWithSourceText st (pprPrimWord64 w)
    ppr (XLit x)            = case ghcPass @p of
      GhcTc -> case x of
         (HsInteger st i _) -> pprWithSourceText st (integer i)
         (HsRat  f _)       -> ppr f

pprHsStringLit :: SourceText -> FastString -> SDoc
pprHsStringLit NoSourceText     s = pprHsString s
pprHsStringLit (SourceText src) _ = vcat $ map text $ split '\n' (unpackFS src)

-- -----------------------------------------------------------------------------
-- HsOverLit

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module GHC.HsToCore.Match.Literal)
instance Eq (HsOverLit (GhcPass p)) where
  (OverLit _ val1) == (OverLit _ val2) = val1 == val2

instance Ord (HsOverLit (GhcPass p)) where
  compare (OverLit _ val1)  (OverLit _ val2) = val1 `compare` val2

-- in debug mode, print the expression that it's resolved to, too
instance OutputableBndrId p
       => Outputable (HsOverLit (GhcPass p)) where
  ppr (OverLit {..})
        = ppr ol_val <+> (whenPprDebug (parens (pprXOverLit (ghcPass @p) ol_ext)))

-- -----------------------------------------------------------------------------
-- OverLitVal

instance Eq (OverLitVal (GhcPass p)) where
  HsIntegral   i1 == HsIntegral   i2 = i1 == i2
  HsIntegral{}    == _               = False
  HsFractional f1 == HsFractional f2 = f1 == f2
  HsFractional{}  == _               = False
  HsIsString   s1 == HsIsString   s2 = s1 == s2
  HsIsString{}    == _               = False

instance Ord (OverLitVal (GhcPass p)) where
  -- HsIntegral
  HsIntegral i1   `compare` HsIntegral i2   = i1 `compare` i2
  HsIntegral{}    `compare` HsFractional{}  = LT
  HsIntegral{}    `compare` HsIsString{}    = LT
  -- HsFractional
  HsFractional{}  `compare` HsIntegral{}    = GT
  HsFractional f1 `compare` HsFractional f2 = f1 `compare` f2
  HsFractional{}  `compare` HsIsString{}    = LT
  -- HsIsString
  HsIsString{}    `compare` HsIntegral{}    = GT
  HsIsString{}    `compare` HsFractional{}  = GT
  HsIsString   s1 `compare` HsIsString   s2 = sl_fs s1 `lexicalCompareFS` sl_fs s2

instance Outputable (OverLitVal (GhcPass p)) where
  ppr (HsIntegral   i) = pprIntegralLit i
  ppr (HsFractional f) = ppr f
  ppr (HsIsString   s) = ppr s

negateOverLitVal :: OverLitVal (GhcPass p) -> OverLitVal (GhcPass p)
negateOverLitVal (HsIntegral i) = HsIntegral (negateIntegralLit i)
negateOverLitVal (HsFractional f) = HsFractional (negateFractionalLit f)
negateOverLitVal _ = panic "negateOverLitVal: argument is not a number"

-- For internal use only. DO NOT EXPORT!
convertOverLitVal :: OverLitVal (GhcPass p) -> OverLitVal (GhcPass p')
convertOverLitVal = \case
  HsFractional f -> HsFractional $ convertFractionalLit f
  HsIntegral   i -> HsIntegral   $ convertIntegralLit   i
  HsIsString   s -> HsIsString   $ convertStringLit     s

-- |
-- Change the GHC pass from the 'Parsed' pass to the 'Renamed' pass.
rnOverLitVal :: OverLitVal GhcPs -> OverLitVal GhcRn
rnOverLitVal = convertOverLitVal

-- |
-- Change the GHC pass from the 'Renamed' pass to the 'Typechecked' pass.
tcOverLitVal :: OverLitVal GhcRn -> OverLitVal GhcTc
tcOverLitVal = convertOverLitVal

-- -----------------------------------------------------------------------------
-- HsQualLit

type instance XQualLit GhcPs = NoExtField
type instance XQualLit GhcRn = LIdP GhcRn
type instance XQualLit GhcTc = DataConCantHappen

type instance XXQualLit (GhcPass _) = DataConCantHappen

instance Eq (HsQualLit (GhcPass p)) where
  QualLit _ m1 v1 == QualLit _ m2 v2 = (m1, v1) == (m2, v2)

instance Ord (HsQualLit (GhcPass p)) where
  -- QualLit
  QualLit _ m1 v1 `compare` QualLit _ m2 v2 = (m1, v1) `compare` (m2, v2)

instance OutputableBndrId p => Outputable (HsQualLit (GhcPass p)) where
  ppr QualLit{..} = text (moduleNameString ql_mod) <> char '.' <> ppr ql_val

-- -----------------------------------------------------------------------------
-- QualLitVal

type instance XQualLitString (GhcPass _) = SourceText
type instance XXQualLitVal   (GhcPass _) = DataConCantHappen

instance Eq (QualLitVal (GhcPass p)) where
  HsQualString _ s1 == HsQualString _ s2 = s1 == s2

instance Ord (QualLitVal (GhcPass p)) where
  HsQualString _ s1 `compare` HsQualString _ s2 = s1 `lexicalCompareFS` s2

instance Outputable (QualLitVal (GhcPass p)) where
  ppr (HsQualString st s) = pprHsStringLit st s

-- -----------------------------------------------------------------------------
-- FractionalLit

type instance XFractionalLit  (GhcPass p) = SourceText
type instance XXFractionalLit (GhcPass p) = DataConCantHappen

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module GHC.HsToCore.Match.Literal)

-- | Compare fractional lits with small exponents for value equality but
--   large values for syntactic equality.
compareFractionalLit :: FractionalLit (GhcPass p) -> FractionalLit (GhcPass p) -> Ordering
compareFractionalLit fl1 fl2
  | fl_exp fl1 < 100 && fl_exp fl2 < 100 && fl_exp fl1 >= -100 && fl_exp fl2 >= -100
    = rationalFromFractionalLit fl1 `compare` rationalFromFractionalLit fl2
  | otherwise = (compare `on` (\x -> (fl_signi x, fl_exp x, fl_exp_base x))) fl1 fl2

-- | Be wary of using this instance to compare for equal *values* when
-- exponents are large. The same value expressed in different syntactic form
-- won't compare as equal when any of the exponents is >= 100.
instance Eq (FractionalLit (GhcPass p)) where
  (==) fl1 fl2 = case compare fl1 fl2 of
          EQ -> True
          _  -> False

-- | Be wary of using this instance to compare for equal *values* when
-- exponents are large. The same value expressed in different syntactic form
-- won't compare as equal when any of the exponents is >= 100.
instance Ord (FractionalLit (GhcPass p)) where
  compare = compareFractionalLit

instance Outputable (FractionalLit (GhcPass p)) where
  ppr (FL{..}) =
    let base = case fl_exp_base of
          Base2 -> 2
          Base10 -> 10
        rat = fl_signi * (base ^^ fl_exp)
    in  pprWithSourceText fl_text $ rational rat

-- The 'Show[ instance is required for the derived
-- 'GHC.Parser.Lexer.Token' instance when DEBUG is enabled.
instance Show (FractionalLit (GhcPass p)) where
  show (FL{..}) = unwords
    [ show fl_text
    , show fl_neg
    , show fl_signi
    , show fl_exp
    , show fl_exp_base
    ]

mkFractionalLit ::
  SourceText ->
  Bool ->
  Rational ->
  Integer ->
  FractionalExponentBase ->
  FractionalLit (GhcPass p)
mkFractionalLit = FL

-- | The arguments should already be negated if they are negative.
mkFractionalLitFromText ::
  String ->
  Bool ->
  Integer ->
  Integer ->
  FractionalExponentBase ->
  FractionalLit (GhcPass p)
mkFractionalLitFromText !str !b !r !i !ff =
  FL (SourceText $ fsLit str) b (r % 1) i ff

-- | The integer should already be negated if it's negative.
mkFractionalLitFromInteger :: Bool -> Integer -> FractionalLit (GhcPass p)
mkFractionalLitFromInteger neg i = FL
  { fl_text = SourceText (fsLit $ show i)
  , fl_neg = neg
  , fl_signi = i % 1
  , fl_exp = 0
  , fl_exp_base = Base10
  }

mkFractionalLitFromRational :: Rational -> FractionalLit (GhcPass p)
mkFractionalLitFromRational r =  FL
  { fl_text = NoSourceText
  , fl_neg = r < 0
  , fl_signi = r
  , fl_exp = 0
  , fl_exp_base = Base10
  }

mkTHFractionalLit :: Rational -> FractionalLit (GhcPass p)
mkTHFractionalLit r =  FL
  { fl_text = SourceText (fsLit $ show (realToFrac r::Double))
    -- Converting to a Double here may technically lose
    -- precision (see #15502). We could alternatively
    -- convert to a Rational for the most accuracy, but
    -- it would cause Floats and Doubles to be displayed
    -- strangely, so we opt not to do this. (In contrast
    -- to mkIntegralLit, where we always convert to an
    -- Integer for the highest accuracy.)
  , fl_neg = r < 0
  , fl_signi = r
  , fl_exp = 0
  , fl_exp_base = Base10
  }

negateFractionalLit :: FractionalLit (GhcPass p) -> FractionalLit (GhcPass p)
negateFractionalLit (FL text neg i e eb) = case text of
  SourceText (unconsFS -> Just ('-',src))
                       -> FL (SourceText src)                False (negate i) e eb
  SourceText      src  -> FL (SourceText ('-' `consFS` src)) True  (negate i) e eb
  NoSourceText         -> FL NoSourceText (not neg) (negate i) e eb

rationalFromFractionalLit :: FractionalLit (GhcPass p) -> Rational
rationalFromFractionalLit (FL _ _ i e expBase) =
  mkRationalWithExponentBase i e expBase

mkRationalWithExponentBase :: Rational -> Integer -> FractionalExponentBase -> Rational
mkRationalWithExponentBase i e feb = i * (eb ^^ e)
  where eb = case feb of Base2 -> 2 ; Base10 -> 10


-- For internal use only. DO NOT EXPORT!
convertFractionalLit :: FractionalLit (GhcPass p) -> FractionalLit (GhcPass p')
convertFractionalLit f = FL
   { fl_text     = fl_text     f
   , fl_neg      = fl_neg      f
   , fl_signi    = fl_signi    f
   , fl_exp      = fl_exp      f
   , fl_exp_base = fl_exp_base f
   }

-- |
-- Change the GHC pass from the 'Parsed' pass to the 'Renamed' pass.
rnFractionalLit :: FractionalLit GhcRn -> FractionalLit GhcTc
rnFractionalLit = convertFractionalLit

-- |
-- Change the GHC pass from the current 'Pass' to the 'Typechecked' pass.
-- This function can safely accept any GHC 'Pass; as the input,
-- because 'Typechecked' is the final 'Pass'. Note that this permits the
-- "no-op" of going from 'Typechecked' to 'Typechecked'.
{-# INLINE[1] tcFractionalLit #-}
{-# RULES "tcFractionalLit/id" tcFractionalLit = id #-}
tcFractionalLit :: FractionalLit (GhcPass p) -> FractionalLit GhcTc
tcFractionalLit = convertFractionalLit

-- -----------------------------------------------------------------------------
-- IntegralLit

type instance XIntegralLit  (GhcPass p) = SourceText
type instance XXIntegralLit (GhcPass p) = DataConCantHappen

instance Eq (IntegralLit (GhcPass p)) where
  (==) x y = il_value x == il_value y

instance Ord (IntegralLit (GhcPass p)) where
  compare x y = il_value x `compare` il_value y

instance Outputable (IntegralLit (GhcPass p)) where
  ppr (IL (SourceText src) _ _) = ftext src
  ppr (IL NoSourceText _ value) = text (show value)

instance Show (IntegralLit (GhcPass p)) where
  show (IL{..}) = unwords [ show il_text, show il_neg, show il_value ]

mkIntegralLit :: Integral a => a -> IntegralLit (GhcPass p)
mkIntegralLit i = IL
  { il_text = SourceText (fsLit $ show i_integer)
  , il_neg = i < 0
  , il_value = i_integer
  }
  where
    i_integer :: Integer
    i_integer = toInteger i

negateIntegralLit :: IntegralLit (GhcPass p) -> IntegralLit (GhcPass p)
negateIntegralLit (IL{..}) = case il_text of
  SourceText (unconsFS -> Just ('-',src)) -> IL (SourceText src)                False (negate il_value)
  SourceText src                          -> IL (SourceText ('-' `consFS` src)) True  (negate il_value)
  NoSourceText                            -> IL NoSourceText             (not il_neg) (negate il_value)

pprIntegralLit :: IntegralLit (GhcPass p) -> SDoc
pprIntegralLit (IL{..}) = pprWithSourceText il_text $ integer il_value

-- For internal use only. DO NOT EXPORT!
convertIntegralLit :: IntegralLit (GhcPass p) -> IntegralLit (GhcPass p')
convertIntegralLit (IL{..}) = IL
  { il_text  = il_text
  , il_neg   = il_neg
  , il_value = il_value
  }

-- |
-- Change the GHC pass from the 'Parsed' pass to the 'Renamed' pass.
rnIntegralLit :: IntegralLit GhcRn -> IntegralLit GhcTc
rnIntegralLit = convertIntegralLit

-- |
-- Change the GHC pass from the current 'Pass' to the 'Typechecked' pass.
-- This function can safely accept any GHC 'Pass; as the input,
-- because 'Typechecked' is the final 'Pass'. Note that this permits the
-- "no-op" of going from 'Typechecked' to 'Typechecked'.
{-# INLINE[1] tcIntegralLit #-}
{-# RULES "tcIntegralLit/id" tcIntegralLit = id #-}
tcIntegralLit :: IntegralLit (GhcPass p) -> IntegralLit GhcTc
tcIntegralLit = convertIntegralLit

-- -----------------------------------------------------------------------------
-- StringLit

type instance XStringLit  (GhcPass p) = SourceText
type instance XXStringLit (GhcPass p) = DataConCantHappen

instance Eq (StringLiteral (GhcPass p)) where
  (StringLiteral _ a) == (StringLiteral _ b) = a == b

instance Outputable (StringLiteral (GhcPass p)) where
  ppr (StringLiteral{..}) = pprWithSourceText sl_src (doubleQuotes $ ftext sl_fs)

-- | Get the 'SourceText' of a 'StringLiteral'
{-# INLINE stringLitSourceText #-}
stringLitSourceText :: StringLiteral (GhcPass p) -> SourceText
stringLitSourceText = sl_src

-- For internal use only. DO NOT EXPORT!
convertStringLit :: StringLiteral (GhcPass p) -> StringLiteral (GhcPass p')
convertStringLit (StringLiteral{..}) = StringLiteral
  { sl_fs  = sl_fs
  , sl_src = sl_src
  }

-- |
-- Change the GHC pass from the 'Parsed' pass to the 'Renamed' pass.
{-# INLINE rnStringLit #-}
rnStringLit :: StringLiteral GhcPs -> StringLiteral GhcRn
rnStringLit = convertStringLit

-- |
-- Change the GHC pass from the current 'Pass' to the 'Typechecked' pass.
-- This function can safely accept any GHC 'Pass; as the input,
-- because 'Typechecked' is the final 'Pass'. Note that this permits the
-- "no-op" of going from 'Typechecked' to 'Typechecked'.
{-# INLINE[1] tcStringLit #-}
{-# RULES "tcStringLit/id" tcStringLit = id #-}
tcStringLit :: StringLiteral (GhcPass p) -> StringLiteral GhcTc
tcStringLit = convertStringLit


-- -----------------------------------------------------------------------------
-- Deprecations

-- These functions persist for backwards compatability:

{-# DEPRECATED fractionalLitFromRational "Prefer mkFractionalLitFromRational" #-}
fractionalLitFromRational :: Rational -> FractionalLit (GhcPass p)
fractionalLitFromRational = mkFractionalLitFromRational

{-# DEPRECATED integralFractionalLit "Prefer mkFractionalLitFromInteger" #-}
integralFractionalLit :: Bool -> Integer -> FractionalLit (GhcPass p)
integralFractionalLit = mkFractionalLitFromInteger

{-# DEPRECATED mkSourceFractionalLit "Prefer mkFractionalLitFromText" #-}
mkSourceFractionalLit ::
  String ->
  Bool ->
  Integer ->
  Integer ->
  FractionalExponentBase ->
  FractionalLit (GhcPass p)
mkSourceFractionalLit = mkFractionalLitFromText
