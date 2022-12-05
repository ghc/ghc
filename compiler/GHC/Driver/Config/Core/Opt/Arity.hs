module GHC.Driver.Config.Core.Opt.Arity
  ( initArityOpts
  ) where

import GHC.Prelude (not)

import GHC.Driver.Session

import GHC.Core.Opt.Arity
import GHC.Types.Basic (StateHackFlag(..))

initArityOpts :: DynFlags -> ArityOpts
initArityOpts dflags = ArityOpts
  { ao_ped_bot = gopt Opt_PedanticBottoms dflags
  , ao_dicts_cheap = gopt Opt_DictsCheap dflags
  , ao_state_hack = StateHackFlag (not (gopt Opt_G_NoStateHack dflags))
  }
