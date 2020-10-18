{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module T14933 where

import Data.Kind (Type)
import Data.Type.Equality (type (~))

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
