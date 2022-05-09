module GHC.Cmm.Parser.Config (
    PDConfig(..)
  , CmmParserConfig(..)
) where

import GHC.Prelude

import GHC.Platform.Profile

import GHC.StgToCmm.Config

import GHC.Parser.Lexer


data PDConfig = PDConfig
  { pdProfile :: !Profile
  , pdSanitizeAlignment :: !Bool -- ^ Insert alignment checks (cf @-falignment-sanitisation@)
  }

data CmmParserConfig = CmmParserConfig
  { cmmpParserOpts :: !ParserOpts
  , cmmpPDConfig :: !PDConfig
  , cmmpStgToCmmConfig :: !StgToCmmConfig
  }
