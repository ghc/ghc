{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Source-language literals
module Language.Haskell.Syntax.Lit (
  -- * Literal Syntax
  -- ** Normal Haskell Literal
  -- *** Data-type
    HsLit(..)
  -- ** Overloaded Haskell Literal
  -- *** Data-types
  , HsOverLit(..)
  , OverLitVal(..)
  -- ** Haskell Qualified Literal
  , HsQualLit(..)
  , QualLitVal(..)

  -- * Literal Values
  -- ** Fractional
  -- *** Data-types
  , FractionalLit(..)
  , FractionalExponentBase(..)

  -- ** Integral
  -- *** Data-type
  , IntegralLit(..)

  -- ** Textual
  -- *** Data-type
  , StringLiteral(..)
  -- *** Located synonym
  , LStringLit
  ) where

import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Module.Name (ModuleName)

import GHC.Data.FastString

import Data.Bool
import Data.ByteString (ByteString)
import Data.Char
import Data.Data hiding (Fixity)
import Data.Eq
import Data.Ord
import Prelude

{-
************************************************************************
*                                                                      *
\subsection[HsLit]{Literals}
*                                                                      *
************************************************************************
-}

-- Note [Trees That Grow] in "Language.Haskell.Syntax.Extension" for the Xxxxx
-- fields in the following
-- | Haskell Literal
data HsLit x
  = HsChar (XHsChar x) Char
      -- ^ Character
  | HsCharPrim (XHsCharPrim x) Char
      -- ^ Unboxed character
  | HsString (XHsString x) FastString
      -- ^ String
  | HsStringPrim (XHsStringPrim x) !ByteString
      -- ^ Packed bytes
  | HsDouble (XHsDouble x) (FractionalLit x)
      -- ^ Genuinely a @Double@; used in type errors only
  | HsNatural (XHsNatural x) (IntegralLit x)
      -- ^ Genuinely a @Natural@; used in types only
  | HsInt (XHsInt x) (IntegralLit x)
      -- ^ Genuinely an Int; arises from
      -- "GHC.Tc.Deriv.Generate", and from TRANSLATION
  | HsIntPrim (XHsIntPrim x) Integer
      -- ^ literal @Int#@
  | HsWordPrim (XHsWordPrim x) Integer
      -- ^ literal @Word#@
  | HsInt8Prim (XHsInt8Prim x) Integer
      -- ^ literal @Int8#@
  | HsInt16Prim (XHsInt16Prim x) Integer
      -- ^ literal @Int16#@
  | HsInt32Prim (XHsInt32Prim x) Integer
      -- ^ literal @Int32#@
  | HsInt64Prim (XHsInt64Prim x) Integer
      -- ^ literal @Int64#@
  | HsWord8Prim (XHsWord8Prim x) Integer
      -- ^ literal @Word8#@
  | HsWord16Prim (XHsWord16Prim x) Integer
      -- ^ literal @Word16#@
  | HsWord32Prim (XHsWord32Prim x) Integer
      -- ^ literal @Word32#@
  | HsWord64Prim (XHsWord64Prim x) Integer
      -- ^ literal @Word64#@
  | HsFloatPrim (XHsFloatPrim x)   (FractionalLit x)
      -- ^ Unboxed Float
  | HsDoublePrim (XHsDoublePrim x) (FractionalLit x)
      -- ^ Unboxed Double
  | XLit !(XXLit x)

-- | Haskell Overloaded Literal
data HsOverLit p
  = OverLit
  { ol_ext :: (XOverLit p)
  , ol_val :: OverLitVal p
  }
  | XOverLit !(XXOverLit p)

-- | Overloaded Literal Value
data OverLitVal x
  = HsIntegral   !(IntegralLit   x) -- ^ Integer-looking literals
  | HsFractional !(FractionalLit x) -- ^    Frac-looking literals
  | HsIsString   !(StringLiteral x) -- ^  String-looking literals

-- | Haskell Qualified Literal
data HsQualLit p
  = QualLit
      { ql_ext :: !(XQualLit p)
      , ql_mod :: !ModuleName
      , ql_val :: !(QualLitVal p)
      }
  | XQualLit !(XXQualLit p)

data QualLitVal p
  = HsQualString !(XQualLitString p) !FastString
    -- ^ See Note [Implementation of QualifiedStrings]
  | XQualLitVal !(XXQualLitVal p)

------------------------------------------------
-- Literals
------------------------------------------------

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
  { fl_text      :: XFractionalLit pass
  , fl_neg      :: Bool                   -- See Note [Negative zero]
  , fl_signi    :: Rational               -- The significand component of the literal
  , fl_exp      :: Integer                -- The exponent component of the literal
  , fl_exp_base :: FractionalExponentBase -- See Note [fractional exponent bases]
  }
  | XFractionalLit !(XXFractionalLit pass)

{- Note [fractional exponent bases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For hexadecimal rationals of
the form 0x0.3p10 the exponent is given on base 2 rather than
base 10. These are the only options, hence the sum type. See also #15646.
-}

-- See Note [FractionalLit representation] in GHC.HsToCore.Match.Literal
data FractionalExponentBase
  = Base2 -- Used in hex fractional literals
  | Base10
  deriving (Eq, Ord, Data, Show)

-- | Integral Literal
--
-- Used (instead of Integer) to represent negative zegative zero which is
-- required for NegativeLiterals extension to correctly parse `-0::Double`
-- as negative zero. See also #13211.
data IntegralLit pass = IL
  { il_text :: (XIntegralLit pass)
  , il_neg :: Bool -- See Note [Negative zero] in GHC.Rename.Pat
  , il_value :: Integer
  }
  | XIntegralLit !(XXIntegralLit pass)

-- | Located Haskell String Literal
type LStringLit p = XRec p (StringLiteral p)

-- | A String Literal in the source, including its original raw format for use by
-- source to source manipulation tools.
data StringLiteral pass = StringLiteral
  { sl_src :: XStringLit pass
  , sl_fs  :: FastString -- literal string value
  }
  | XStringLit !(XXStringLit pass)
