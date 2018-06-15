{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module NatVal where

import Data.Proxy
import GHC.TypeLits

-- test that Nat type literals are statically converted into Integer literals

foo :: Integer
foo = natVal $ Proxy @0
