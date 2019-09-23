{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeApplications #-}

module ExplicitForAllRules1 where

import Data.Proxy
import Data.Kind

-- From Proposal 0007 (w/ fix to "example")

{-# RULES
"example"  forall a b. forall. map @a @b f = f
"example2" forall a. forall (x :: a). id' x = x
  #-}

{-# NOINLINE f #-}
f :: a -> b
f = undefined

id' :: a -> a
id' x = x
{-# NOINLINE id' #-}

-- More tests

{-# RULES
"example3" forall (a :: Type -> Type) (b :: a Int) c. forall x y. g @(Proxy b) @(Proxy c) x y = ()
"example4" forall (a :: Bool) (b :: Proxy a). forall x. g @(Proxy b) @() x = id' @()
"example5" forall (a :: Type). forall. h @a = id' @a
"example5" forall k (c :: k). forall (x :: Proxy c). id' @(Proxy c) x = x
  #-}

{-# NOINLINE g #-}
g :: a -> b -> ()
g _ _ = ()

{-# NOINLINE h #-}
h :: a -> a
h x = x

-- Should NOT have a parse error :(
{-# RULES "example6" forall a forall. g a forall = () #-}

-- Should generate a warning
{-# RULES "example7" forall a b. forall (x :: a). id' x = x #-}
