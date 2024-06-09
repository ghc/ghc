{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Source text
--
-- Keeping Source Text for source to source conversions
--
module GHC.Types.SourceText
   ( SourceText (..)
   , NoCommentsLocation
   , pprWithSourceText

   -- * Literals
   , IntegralLit(..)
   , FractionalLit(..)
   , StringLit(..)
   , rationalFromFractionalLit
   , FractionalExponentBase(..)

   )
where

import GHC.Prelude

import GHC.Data.FastString

import GHC.Utils.Outputable
import GHC.Utils.Binary
import GHC.Utils.Panic

import Data.Function (on)
import Data.Data
import GHC.Types.SrcLoc
import Control.DeepSeq

import Language.Haskell.Syntax.Extension

{-
Note [Pragma source text]
~~~~~~~~~~~~~~~~~~~~~~~~~
The lexer does a case-insensitive match for pragmas, as well as
accepting both UK and US spelling variants.

So

  {-# SPECIALISE #-}
  {-# SPECIALIZE #-}
  {-# Specialize #-}

will all generate ITspec_prag token for the start of the pragma.

In order to be able to do source to source conversions, the original
source text for the token needs to be preserved, hence the
`SourceText` field.

So the lexer will then generate

  ITspec_prag "{ -# SPECIALISE"
  ITspec_prag "{ -# SPECIALIZE"
  ITspec_prag "{ -# Specialize"

for the cases above.
 [without the space between '{' and '-', otherwise this comment won't parse]


Note [Literal source text]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The lexer/parser converts literals from their original source text
versions to an appropriate internal representation. This is a problem
for tools doing source to source conversions, so the original source
text is stored in literals where this can occur.

Motivating examples for HsLit

  HsChar          '\n'       == '\x20'
  HsCharPrim      '\x41'#    == 'A'#
  HsString        "\x20\x41" == " A"
  HsStringPrim    "\x20"#    == " "#
  HsInt           001        == 1
  HsIntPrim       002#       == 2#
  HsWordPrim      003##      == 3##
  HsInt64Prim     004#Int64  == 4#Int64
  HsWord64Prim    005#Word64 == 5#Word64
  HsInteger       006        == 6

For OverLitVal

  HsIntegral      003      == 0x003
  HsIsString      "\x41nd" == "And"
-}

 -- Note [Literal source text],[Pragma source text]
data SourceText
   = SourceText FastString
   | NoSourceText
      -- ^ For when code is generated, e.g. TH,
      -- deriving. The pretty printer will then make
      -- its own representation of the item.
   deriving (Data, Show, Eq )

instance Outputable SourceText where
  ppr (SourceText s) = text "SourceText" <+> ftext s
  ppr NoSourceText   = text "NoSourceText"

instance NFData SourceText where
    rnf = \case
        SourceText s -> rnf s
        NoSourceText -> ()

instance Binary SourceText where
  put_ bh NoSourceText = putByte bh 0
  put_ bh (SourceText s) = do
        putByte bh 1
        put_ bh s

  get bh = do
    h <- getByte bh
    case h of
      0 -> return NoSourceText
      1 -> do
        s <- get bh
        return (SourceText s)
      _ -> panic $ "Binary SourceText:" ++ show h

-- | Special combinator for showing string literals.
pprWithSourceText :: SourceText -> SDoc -> SDoc
pprWithSourceText NoSourceText     d = d
pprWithSourceText (SourceText src) _ = ftext src

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

deriving instance (Data pass, XIntegralLit pass ~ SourceText)
  => Data (IntegralLit pass)

deriving instance (XIntegralLit pass ~ SourceText) => Show (IntegralLit pass)

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

deriving instance (Data pass, XFractionalLit pass ~ SourceText)
  => Data (FractionalLit pass)

-- | The Show instance is required for the derived GHC.Parser.Lexer.Token instance when DEBUG is on
deriving instance (XFractionalLit pass ~ SourceText) => Show (FractionalLit pass)

-- See Note [FractionalLit representation] in GHC.HsToCore.Match.Literal
data FractionalExponentBase
  = Base2 -- Used in hex fractional literals
  | Base10
  deriving (Eq, Ord, Data, Show)

mkRationalWithExponentBase :: Rational -> Integer -> FractionalExponentBase -> Rational
mkRationalWithExponentBase i e feb = i * (eb ^^ e)
  where eb = case feb of Base2 -> 2 ; Base10 -> 10

rationalFromFractionalLit :: FractionalLit pass -> Rational
rationalFromFractionalLit (FL _ _ i e expBase) =
  mkRationalWithExponentBase i e expBase

{- Note [fractional exponent bases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For hexadecimal rationals of
the form 0x0.3p10 the exponent is given on base 2 rather than
base 10. These are the only options, hence the sum type. See also #15646.
-}


-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module GHC.HsToCore.Match.Literal)

instance Eq (IntegralLit pass) where
  (==) = (==) `on` il_value

instance Ord (IntegralLit pass) where
  compare = compare `on` il_value

instance (XIntegralLit pass ~ SourceText) => Outputable (IntegralLit pass) where
  ppr (IL (SourceText src) _ _) = ftext src
  ppr (IL NoSourceText _ value) = text (show value)


-- | Compare fractional lits with small exponents for value equality but
--   large values for syntactic equality.
compareFractionalLit :: FractionalLit pass -> FractionalLit pass -> Ordering
compareFractionalLit fl1 fl2
  | fl_exp fl1 < 100 && fl_exp fl2 < 100 && fl_exp fl1 >= -100 && fl_exp fl2 >= -100
    = rationalFromFractionalLit fl1 `compare` rationalFromFractionalLit fl2
  | otherwise = (compare `on` (\x -> (fl_signi x, fl_exp x, fl_exp_base x))) fl1 fl2

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

instance (XFractionalLit pass ~ SourceText)
  => Outputable (FractionalLit pass) where
  ppr (fl@(FL {})) =
    pprWithSourceText (fl_text fl) $
      rational $ mkRationalWithExponentBase (fl_signi fl) (fl_exp fl) (fl_exp_base fl)

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

deriving instance (Data pass, XStringLit pass ~ (SourceText, Maybe NoCommentsLocation))
  => Data (StringLit pass)

instance (XStringLit pass ~ (SourceText, Maybe NoCommentsLocation))
  => Outputable (StringLit pass) where
  ppr sl = pprWithSourceText (fst (sl_st sl)) (doubleQuotes $ ftext $ sl_fs sl)
