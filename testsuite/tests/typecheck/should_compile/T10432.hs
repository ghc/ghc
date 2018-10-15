{-# LANGUAGE ExistentialQuantification, PolyKinds,
             DataKinds, RankNTypes, GADTs, TypeOperators #-}
module T10432 where

import Data.Kind (Type)
import Data.Type.Equality

data WrappedType = forall a. WrapType a;

matchReflK :: forall ka kb (a :: ka) (b :: kb) (r :: Type).
  ('WrapType a :~: 'WrapType b) -> (('WrapType a ~ 'WrapType b) => r) -> r;
  matchReflK Refl r = r;

matchReflK2 :: forall ka kb (a :: ka) (b :: kb) (r :: Type).
               ('WrapType a :~: 'WrapType b) ->  r
matchReflK2 x = let foo :: ('WrapType a ~ 'WrapType b) => r
                    foo = undefined
                in undefined
