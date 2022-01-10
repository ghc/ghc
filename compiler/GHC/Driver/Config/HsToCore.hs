module GHC.Driver.Config.HsToCore
  ( initBangOpts
  )
where

import GHC.Types.Id.Make
import GHC.Driver.Session
import qualified GHC.LanguageExtensions as LangExt

initBangOpts :: DynFlags -> BangOpts
initBangOpts dflags = BangOpts
  { bang_opt_strict_data   = xopt LangExt.StrictData dflags
  , bang_opt_unbox_disable = gopt Opt_OmitInterfacePragmas dflags
      -- Don't unbox if we aren't optimising; rather arbitrarily,
      -- we use -fomit-iface-pragmas as the indication
  , bang_opt_unbox_strict  = gopt Opt_UnboxStrictFields dflags
  , bang_opt_unbox_small   = gopt Opt_UnboxSmallStrictFields dflags
  }

