{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}
module Lib where

import Data.Type.Equality
import Data.Functor.Identity
import Data.Void

f :: a :~: Int -> a :~: Bool -> ()
f !_ x = case x of {}

g :: Identity (a :~: Int) -> a :~: Bool -> ()
g (Identity _) Refl = ()

data SMaybe a = SNothing
              | SJust !a

-- | Exhaustive. Note how in addition to @{(a,b) | b /~ True}@, the value set
-- @{(a,b) | y /~ SNothing, b ~ True}@ flows into the next equation, but @y@ is
-- no longer in scope. Normally, we have no way of matching on that without a
-- wildcard match, but in this case we refute @y ~ SJust z@ by unleashing type
-- evidence saying that @z@ must be 'Void' by matching on 'Refl'.
h :: forall a. a :~: Void -> Bool -> ()
h _     True  | let y = undefined :: SMaybe a, SNothing <- y = ()
h Refl  False                                    = ()
