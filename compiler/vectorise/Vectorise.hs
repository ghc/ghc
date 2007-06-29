module Vectorise( vectorise )
where

#include "HsVersions.h"

import DynFlags
import HscTypes

vectorise :: HscEnv -> ModGuts -> IO ModGuts
vectorise hsc_env guts
  | not (Opt_Vectorise `dopt` dflags) = return guts
  | otherwise                         = return guts
  where
    dflags = hsc_dflags hsc_env

