{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE TypeFamilies      #-}
module T14933 where

class Wrapped s where
  type Unwrapped s :: *

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
