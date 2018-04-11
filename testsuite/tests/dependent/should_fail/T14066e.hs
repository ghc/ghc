{-# LANGUAGE MonoLocalBinds, TypeInType, ScopedTypeVariables #-}

module T14066e where

import Data.Proxy

-- this should fail because the dependency between b and c can't be
-- determined in the type signature
h :: forall a. a -> ()
h x = ()
  where
    j :: forall c b. Proxy a -> Proxy b -> Proxy c -> Proxy b
    j _ (p1 :: Proxy b') (p2 :: Proxy c') = (p1 :: Proxy (b' :: c'))
