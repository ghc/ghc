{-# LANGUAGE ExistentialQuantification, PolyKinds,
             DataKinds, RankNTypes, GADTs, TypeOperators #-}
module T10432 where

import Data.Type.Equality

data WrappedType = forall a. WrapType a;

matchReflK :: forall (a :: ka) (b :: kb) (r :: *).
  ('WrapType a :~: 'WrapType b) -> (('WrapType a ~ 'WrapType b) => r) -> r;
  matchReflK Refl r = r;

matchReflK2 :: forall (a :: ka) (b :: kb) (r :: *).
               ('WrapType a :~: 'WrapType b) ->  r
matchReflK2 x = let foo :: ('WrapType a ~ 'WrapType b) => r
                    foo = undefined
                in undefined
