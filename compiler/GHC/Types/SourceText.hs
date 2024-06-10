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
   )
where

import GHC.Prelude

import GHC.Data.FastString

import GHC.Utils.Outputable
import GHC.Utils.Binary
import GHC.Utils.Panic

import Data.Data
import GHC.Types.SrcLoc
import Control.DeepSeq

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
