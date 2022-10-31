module GHC.Cmm.Parser.Config (
    PDConfig(..)
  , CmmParserConfig(..)
) where

import GHC.Prelude

import GHC.Cmm.Builder.Config
import GHC.Parser.Lexer
import GHC.Platform.Profile

data PDConfig = PDConfig
  { pdProfile :: !Profile
  , pdSanitizeAlignment :: !Bool -- ^ Insert alignment checks (cf @-falignment-sanitisation@)
  }

data CmmParserConfig = CmmParserConfig
  { cmmpParserOpts :: !ParserOpts
  , cmmpPDConfig :: !PDConfig
  , cmmpCmmBuilderConfig :: !CmmBuilderConfig
  }
