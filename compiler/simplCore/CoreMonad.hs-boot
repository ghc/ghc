-- Created this hs-boot file to remove circular dependencies from the use of
-- Plugins. Plugins needs CoreToDo and CoreM types to define core-to-core
-- transformations.
-- However CoreMonad does much more than defining these, and because Plugins are
-- activated in various modules, the imports become circular. To solve this I
-- extracted CoreToDo and CoreM into this file.
-- I needed to write the whole definition of these types, otherwise it created
-- a data-newtype conflict.

module CoreMonad ( CoreToDo, CoreM ) where

import GhcPrelude

import IOEnv ( IOEnv )
import UniqSupply ( UniqSupply )

newtype CoreState = CoreState {
        cs_uniq_supply :: UniqSupply
}

type CoreIOEnv = IOEnv CoreReader

data CoreReader

newtype CoreWriter = CoreWriter {
        cw_simpl_count :: SimplCount
}

data SimplCount

newtype CoreM a
          = CoreM { unCoreM :: CoreState
                                 -> CoreIOEnv (a, CoreState, CoreWriter) }

instance Monad CoreM

data CoreToDo
