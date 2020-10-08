-- | Subsystem configuration
module GHC.Driver.Config
   ( initOptCoercionOpts
   , initSimpleOpts
   , initParserOpts
   )
where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Core.SimpleOpt
import GHC.Core.Coercion.Opt
import GHC.Parser.Lexer

-- | Initialise coercion optimiser configuration from DynFlags
initOptCoercionOpts :: DynFlags -> OptCoercionOpts
initOptCoercionOpts dflags = OptCoercionOpts
   { optCoercionEnabled = not (hasNoOptCoercion dflags)
   }

-- | Initialise Simple optimiser configuration from DynFlags
initSimpleOpts :: DynFlags -> SimpleOpts
initSimpleOpts dflags = SimpleOpts
   { so_uf_opts = unfoldingOpts dflags
   , so_co_opts = initOptCoercionOpts dflags
   }

-- | Extracts the flag information needed for parsing
initParserOpts :: DynFlags -> ParserOpts
initParserOpts =
  mkParserOpts
    <$> warningFlags
    <*> extensionFlags
    <*> safeImportsOn
    <*> gopt Opt_Haddock
    <*> gopt Opt_KeepRawTokenStream
    <*> const True
