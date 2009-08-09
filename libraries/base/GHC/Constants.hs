
module GHC.Constants where

import Prelude

-- We use stage1 here, because that's guaranteed to exist
#include "../../../compiler/stage1/ghc_boot_platform.h"

#include "../../../includes/HaskellConstants.hs"
