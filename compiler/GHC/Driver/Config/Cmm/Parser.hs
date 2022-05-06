module GHC.Driver.Config.Cmm.Parser
  ( initCmmParserConfig
  ) where

import GHC.Cmm.Parser
import GHC.Cmm.Parser.Monad

import GHC.Driver.Config.Parser
import GHC.Driver.Config.StgToCmm
import GHC.Driver.Session

import GHC.Unit.Types


initPDConfig :: DynFlags -> PDConfig
initPDConfig dflags = PDConfig
  { pdProfile = targetProfile dflags
  , pdSanitizeAlignment = gopt Opt_AlignmentSanitisation dflags
  }

initCmmParserConfig :: DynFlags -> Module -> CmmParserConfig
initCmmParserConfig dflags mod = CmmParserConfig
  { cmmpParserOpts = initParserOpts dflags
  , cmmpPDConfig = initPDConfig dflags
  , cmmpStgToCmmConfig = initStgToCmmConfig dflags mod
  }

