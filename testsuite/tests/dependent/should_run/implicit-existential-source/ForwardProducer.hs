{-# LANGUAGE AllowAmbiguousTypes, LinearTypes, ScopedTypeVariables, TypeApplications #-}
module ForwardProducer (failAny, choose, keepAfter, keepTagged, ambiguous, linearPair) where

import Data.Proxy (Proxy(..))

{-# NOINLINE failAny #-}
failAny :: String -> a
failAny msg = error msg

{-# NOINLINE choose #-}
choose :: Bool -> a -> a -> a
choose b x y = if b then x else y

{-# NOINLINE keepAfter #-}
keepAfter :: b -> a -> a
keepAfter ignored value = ignored `seq` value


{-# NOINLINE keepTagged #-}
keepTagged :: forall tag left b a. Proxy tag -> Proxy left -> b -> a -> a
keepTagged Proxy Proxy flag value = flag `seq` value

class Mark b where
    mark :: Proxy b -> Bool

instance Mark Int where
    mark Proxy = True

instance Mark Bool where
    mark Proxy = False

{-# NOINLINE ambiguous #-}
ambiguous :: forall b a. Mark b => () -> a -> a
ambiguous () value = mark (Proxy @b) `seq` value

{-# NOINLINE linearPair #-}
linearPair :: (b %1 -> b %1 -> a %1 -> a) -> b %1 -> b %1 -> a %1 -> a
linearPair use l r value = use l r value
