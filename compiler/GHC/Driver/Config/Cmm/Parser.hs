module GHC.Driver.Config.Cmm.Parser
  ( initCmmParserConfig
  ) where

import GHC.Cmm.Parser.Config

import GHC.Driver.Config.Parser
import GHC.Driver.Config.StgToCmm
import GHC.Driver.Session

import GHC.Utils.Panic

initPDConfig :: DynFlags -> PDConfig
initPDConfig dflags = PDConfig
  { pdProfile = targetProfile dflags
  , pdSanitizeAlignment = gopt Opt_AlignmentSanitisation dflags
  }

initCmmParserConfig :: DynFlags -> CmmParserConfig
initCmmParserConfig dflags = CmmParserConfig
  { cmmpParserOpts = initParserOpts dflags
  , cmmpPDConfig = initPDConfig dflags
  , cmmpStgToCmmConfig = initStgToCmmConfig dflags (panic "initCmmParserConfig: no module")
  }

