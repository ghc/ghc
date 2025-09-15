{-# LANGUAGE PolyKinds, FlexibleInstances, ScopedTypeVariables,
             UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module PolyInstances where

import Data.Kind
import Data.Proxy

class C (a :: k)

instance (C a, C b) => C (a b)

data Dict :: Constraint -> Type

instance C Dict

foo :: C p => proxy p -> ()
foo = undefined

bar :: forall (p :: Constraint) proxy. C p => proxy p -> ()
bar _ = foo (Proxy :: Proxy (Dict p))
