{-# LANGUAGE
     FlexibleContexts
   , FlexibleInstances
   , FunctionalDependencies
   , MultiParamTypeClasses
   , KindSignatures
   , UndecidableInstances #-}

module T7875 where

class Het a b | a -> b where
  het :: m (f c) -> a -> m b

class GHet (a :: * -> *) (b :: * -> *) | a -> b
instance            GHet (K a) (K [a])
instance Het a b => GHet (K a) (K b)


data A a   = A (A a)
data K x a = K x

instance Het (A a) (A [a]) where het = het1

het1 :: (GHet (K a) (K b)) => m (f c) -> a -> m b
het1 = undefined


{- Wanted
     (GHet (K (A a)) (K (A [a])))

-- Fundeps give ([A a] ~ A [a])
-}