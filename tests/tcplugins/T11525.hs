{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies,
             ConstraintKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin T11525_Plugin #-}
module T11525 where

import GHC.TypeLits
import Data.Proxy

truncateB :: KnownNat a => Proxy (a + b) -> Proxy a
truncateB Proxy = Proxy

class Bus t where
  type AddrBits t :: Nat

data MasterOut b = MasterOut
    { adr :: Proxy (AddrBits b)
    }

type WiderAddress b b' k = ( KnownNat (AddrBits b)
                           , AddrBits b' ~ (AddrBits b + k)
                           )

narrowAddress' :: (WiderAddress b b' k)
               => MasterOut b'
               -> MasterOut b
narrowAddress' m = MasterOut { adr = truncateB (adr m) }
