{-# LANGUAGE PolyKinds, DataKinds, ScopedTypeVariables #-}

module T21088 where

import Data.Proxy
  ( Proxy(..) )
import GHC.Exts
  ( TYPE, RuntimeRep )

-- We don't change the order of quantification,
-- so we check we are not instantiating `r1` but not `r2`,
-- which would be quite confusing.
foo :: forall {r1 :: RuntimeRep} (a1 :: TYPE r1)
              {r2 :: RuntimeRep} (a2 :: TYPE r2)
    .  Proxy a1 -> Proxy a2
foo _ = Proxy

bar :: forall {r1 :: RuntimeRep} {r2 :: RuntimeRep}
              (a1 :: TYPE r1)    (a2 :: TYPE r2)
    .  Proxy a1 -> Proxy a2
bar _ = Proxy

baz :: forall {k1} (a1 :: k1) {k2} (a2 :: k2)
    .  Proxy a1 -> Proxy a2
baz _ = Proxy

quux :: forall {k1} {k2} (a1 :: k1) (a2 :: k2)
     .  Proxy a1 -> Proxy a2
quux _ = Proxy
