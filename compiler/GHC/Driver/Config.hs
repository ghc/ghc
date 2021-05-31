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

-- | Initialise coercion optimiser configuration from DynFlags, for use when
-- performing "simple" coercion optimisation.
-- See Note [Simple and full coercion optimisation] in GHC.Core.Coercion.Opt.
initOptCoercionOpts :: DynFlags -> OptCoercionOpts
initOptCoercionOpts dflags
  | gopt Opt_OptCoercionSimple dflags = SimpleCoercionOpt
  | otherwise                         = NoCoercionOpt

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
