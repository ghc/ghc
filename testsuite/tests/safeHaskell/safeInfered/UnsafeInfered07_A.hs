-- | Unsafe as hand crafts a typeable instance
module UnsafeInfered07_A where

import Data.Typeable

data G = G Int

instance Typeable G where
    typeOf _ = typeOf (undefined::Int)

