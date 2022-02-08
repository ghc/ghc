module GHC.Driver.Config.Core.Opt.Arity
  ( initArityOpts
  ) where

import GHC.Prelude ()

import GHC.Driver.Session

import GHC.Core.Opt.Arity

initArityOpts :: DynFlags -> ArityOpts
initArityOpts dflags = ArityOpts
  { ao_ped_bot = gopt Opt_PedanticBottoms dflags
  , ao_dicts_cheap = gopt Opt_DictsCheap dflags
  }
