-- TODO: Deprecate
module GHC.Constants where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Internal.Types () -- for build ordering
