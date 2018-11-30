{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Foo where

import Data.Kind
import Data.Proxy

type family T1 (x :: f (a :: Type))

class C (a :: Type) where
  type T2 (x :: f a)

class C2 (a :: Type) (b :: Proxy a) (c :: Proxy b) where
  type T3 (x :: Proxy '(a, (c :: Proxy b)))
     -- NB: we have to put (c :: Proxy b) so that 'b' is Specified
     --     in the kind of T3; else 'b' is Inferred and comes
     --     first, which is ill-scoped

-- no CUSK
class C3 (a :: Type) (b :: Proxy a) (c :: Proxy b) d where
  type T4 (x :: Proxy '(a, (c :: Proxy b)))
     -- Ditto to T3

class C4 (a :: Type) b where
  type T5 (x :: f a)

class C5 a where
  type T6 (x :: f a)
