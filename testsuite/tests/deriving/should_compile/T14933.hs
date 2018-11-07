{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE TypeFamilies      #-}
module T14933 where

import Data.Kind (Type)

class Wrapped s where
  type Unwrapped s :: Type

class Fork m where
    fork :: (x, m)

    default fork :: ( Wrapped m
                    , Unwrapped m ~ t
                    , Fork t
                    ) => (x, m)
    fork = undefined

newtype MyThing m = MyThing m
    deriving Fork

instance Wrapped (MyThing m) where
    type Unwrapped (MyThing m) = m
