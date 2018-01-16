{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module T12845 where

import Data.Proxy

data Foo (m :: Bool)

type family Head (xs :: [(Bool, Bool)]) where Head (x ': xs) = x

type family Bar (x :: Bool) (y :: Bool) :: Bool

-- to trigger the bug, r and r' cannot *both* appear on the RHS
broken :: forall r r' rngs . ('(r,r') ~ Head rngs, Bar r r' ~ 'True, _)
  => Foo r -> Proxy rngs -> ()
broken x _ = let y = requireBar x :: Foo r' in ()

requireBar :: (Bar m m' ~ 'True) => Foo m -> Foo m'
requireBar = undefined
