{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module T13123 where

import Data.Kind (Type, Constraint)

$([d| idProxy :: forall k proxy (a :: k). proxy a -> proxy a
      idProxy x = x
    |])

$([d| id2 :: Show a => a -> a
      id2 x = x
      {-# SPECIALIZE id2 :: forall k proxy (a :: k). Show (proxy a)
                         => proxy a -> proxy a #-}
    |])

$([d| wibble :: Maybe Int
      wibble = (undefined :: forall k proxy (a :: k). proxy a)
    |])

$([d| class Foo b where
        bar         :: forall k proxy (a :: k). proxy a -> b
        default bar :: forall k proxy (a :: k). proxy a -> b
        bar = undefined
    |])

$([d| data GADT where
        MkGADT :: forall k proxy (a :: k). proxy a -> GADT
    |])

$([d| data Dec13 :: (Type -> Constraint) -> Type where
        MkDec13 :: c a => a -> Dec13 c
    |])
