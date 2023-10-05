{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE
     FlexibleContexts
   , FlexibleInstances
   , FunctionalDependencies
   , MultiParamTypeClasses
   , KindSignatures
   , UndecidableInstances #-}

module T7875 where

import Data.Kind (Type)

class Het a b | a -> b where
  het :: m (f c) -> a -> m b

class GHet (a :: Type -> Type) (b :: Type -> Type) | a -> b
instance            GHet (K a) (K [a])
instance Het a b => GHet (K a) (K b)


data A a   = A (A a)
data K x a = K x

instance Het (A a) (A [a]) where het = het1

het1 :: (GHet (K a) (K b)) => m (f c) -> a -> m b
-- Weird test case: (GHet (K a) (K b)) is simplifiable
het1 = undefined


{- Wanted
     (GHet (K (A a)) (K (A [a])))

-- Fundeps give ([A a] ~ A [a])
-}
