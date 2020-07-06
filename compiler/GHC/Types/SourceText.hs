{-# LANGUAGE DeriveDataTypeable #-}

-- | Source text
--
-- Keeping Source Text for source to source conversions
--
module GHC.Types.SourceText
   ( SourceText (..)
   , pprWithSourceText

   -- * Literals
   , IntegralLit(..)
   , FractionalLit(..)
   , StringLiteral(..)
   , negateIntegralLit
   , negateFractionalLit
   , mkIntegralLit
   , mkFractionalLit
   , integralFractionalLit
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

  HsChar          '\n'       == '\x20`
  HsCharPrim      '\x41`#    == `A`
  HsString        "\x20\x41" == " A"
  HsStringPrim    "\x20"#    == " "#
  HsInt           001        == 1
  HsIntPrim       002#       == 2#
  HsWordPrim      003##      == 3##
  HsInt64Prim     004##      == 4##
  HsWord64Prim    005##      == 5##
  HsInteger       006        == 6

For OverLitVal

  HsIntegral      003      == 0x003
  HsIsString      "\x41nd" == "And"
-}

 -- Note [Literal source text],[Pragma source text]
data SourceText
   = SourceText String
   | NoSourceText
      -- ^ For when code is generated, e.g. TH,
      -- deriving. The pretty printer will then make
      -- its own representation of the item.
   deriving (Data, Show, Eq )

instance Outputable SourceText where
  ppr (SourceText s) = text "SourceText" <+> text s
  ppr NoSourceText   = text "NoSourceText"

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
pprWithSourceText (SourceText src) _ = text src

------------------------------------------------
-- Literals
------------------------------------------------

-- | Integral Literal
--
-- Used (instead of Integer) to represent negative zegative zero which is
-- required for NegativeLiterals extension to correctly parse `-0::Double`
-- as negative zero. See also #13211.
data IntegralLit = IL
   { il_text  :: SourceText
   , il_neg   :: Bool -- See Note [Negative zero] in GHC.Rename.Pat
   , il_value :: Integer
   }
   deriving (Data, Show)

mkIntegralLit :: Integral a => a -> IntegralLit
mkIntegralLit i = IL { il_text = SourceText (show i_integer)
                     , il_neg = i < 0
                     , il_value = i_integer }
  where
    i_integer :: Integer
    i_integer = toInteger i

negateIntegralLit :: IntegralLit -> IntegralLit
negateIntegralLit (IL text neg value)
  = case text of
      SourceText ('-':src) -> IL (SourceText src)       False    (negate value)
      SourceText      src  -> IL (SourceText ('-':src)) True     (negate value)
      NoSourceText         -> IL NoSourceText          (not neg) (negate value)

-- | Fractional Literal
--
-- Used (instead of Rational) to represent exactly the floating point literal that we
-- encountered in the user's source program. This allows us to pretty-print exactly what
-- the user wrote, which is important e.g. for floating point numbers that can't represented
-- as Doubles (we used to via Double for pretty-printing). See also #2245.
data FractionalLit = FL
   { fl_text :: SourceText     -- ^ How the value was written in the source
   , fl_neg :: Bool            -- ^ See Note [Negative zero] in GHC.Rename.Pat
   , fl_value :: Rational      -- ^ Numeric value of the literal
   }
   deriving (Data, Show)
  -- The Show instance is required for the derived GHC.Parser.Lexer.Token instance when DEBUG is on

mkFractionalLit :: Real a => a -> FractionalLit
mkFractionalLit r = FL { fl_text = SourceText (show (realToFrac r::Double))
                           -- Converting to a Double here may technically lose
                           -- precision (see #15502). We could alternatively
                           -- convert to a Rational for the most accuracy, but
                           -- it would cause Floats and Doubles to be displayed
                           -- strangely, so we opt not to do this. (In contrast
                           -- to mkIntegralLit, where we always convert to an
                           -- Integer for the highest accuracy.)
                       , fl_neg = r < 0
                       , fl_value = toRational r }

negateFractionalLit :: FractionalLit -> FractionalLit
negateFractionalLit (FL text neg value)
  = case text of
      SourceText ('-':src) -> FL (SourceText src)     False value
      SourceText      src  -> FL (SourceText ('-':src)) True  value
      NoSourceText         -> FL NoSourceText (not neg) (negate value)

integralFractionalLit :: Bool -> Integer -> FractionalLit
integralFractionalLit neg i = FL { fl_text = SourceText (show i),
                                   fl_neg = neg,
                                   fl_value = fromInteger i }

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module GHC.HsToCore.Match.Literal)

instance Eq IntegralLit where
  (==) = (==) `on` il_value

instance Ord IntegralLit where
  compare = compare `on` il_value

instance Outputable IntegralLit where
  ppr (IL (SourceText src) _ _) = text src
  ppr (IL NoSourceText _ value) = text (show value)

instance Eq FractionalLit where
  (==) = (==) `on` fl_value

instance Ord FractionalLit where
  compare = compare `on` fl_value

instance Outputable FractionalLit where
  ppr f = pprWithSourceText (fl_text f) (rational (fl_value f))

-- | A String Literal in the source, including its original raw format for use by
-- source to source manipulation tools.
data StringLiteral = StringLiteral
                       { sl_st :: SourceText, -- literal raw source.
                                              -- See not [Literal source text]
                         sl_fs :: FastString, -- literal string value
                         sl_tc :: Maybe RealSrcSpan -- Location of
                                                    -- possible
                                                    -- trailing comma
                       -- AZ: if we could have a LocatedA
                       -- StringLiteral we would not need sl_tc, but
                       -- that would cause import loops.

                       } deriving Data

instance Eq StringLiteral where
  (StringLiteral _ a _) == (StringLiteral _ b _) = a == b

instance Outputable StringLiteral where
  ppr sl = pprWithSourceText (sl_st sl) (ftext $ sl_fs sl)

instance Binary StringLiteral where
  put_ bh (StringLiteral st fs _) = do
            put_ bh st
            put_ bh fs
  get bh = do
            st <- get bh
            fs <- get bh
            return (StringLiteral st fs Nothing)
