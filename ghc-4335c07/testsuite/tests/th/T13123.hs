{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module T13123 where

import GHC.Exts (Constraint)

{-
$([d| idProxy :: forall proxy (a :: k). proxy a -> proxy a
      idProxy x = x
    |])

$([d| id2 :: Show a => a -> a
      id2 x = x
      {-# SPECIALIZE id2 :: forall proxy (a :: k). Show (proxy a)
                         => proxy a -> proxy a #-}
    |])

$([d| wibble :: Maybe Int
      wibble = (undefined :: forall proxy (a :: k). proxy a)
    |])

$([d| class Foo b where
        bar         :: forall proxy (a :: k). proxy a -> b
        default bar :: forall proxy (a :: k). proxy a -> b
        bar = undefined
    |])

$([d| data GADT where
        MkGADT :: forall proxy (a :: k). proxy a -> GADT
    |])
-}

$([d| data Dec13 :: (* -> Constraint) -> * where
        MkDec13 :: c a => a -> Dec13 c
    |])
