-- Created this hs-boot file to remove circular dependencies from the use of
-- Plugins. Plugins needs CoreToDo and CoreM types to define core-to-core
-- transformations.
-- However GHC.Core.Opt.Monad does much more than defining these, and because Plugins are
-- activated in various modules, the imports become circular. To solve this I
-- extracted CoreToDo and CoreM into this file.
-- I needed to write the whole definition of these types, otherwise it created
-- a data-newtype conflict.

module GHC.Core.Opt.Monad ( CoreToDo, CoreM ) where

import GHC.Prelude

import GHC.Data.IOEnv ( IOEnv )

type CoreIOEnv = IOEnv CoreReader

data CoreReader

newtype CoreWriter = CoreWriter {
        cw_simpl_count :: SimplCount
}

data SimplCount

newtype CoreM a = CoreM { unCoreM :: CoreIOEnv (a, CoreWriter) }

instance Monad CoreM

data CoreToDo
