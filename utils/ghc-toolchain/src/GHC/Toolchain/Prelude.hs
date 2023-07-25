module GHC.Toolchain.Prelude
    ( module GHC.Toolchain.Monad
    , module GHC.Toolchain.Lens
    , module Prelude
    , (<|>)
    ) where

import GHC.Toolchain.Monad
import GHC.Toolchain.Lens
import Control.Applicative
import Prelude hiding (writeFile, readFile)
