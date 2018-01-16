{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module T7710 where

import Data.Typeable


type T = Int
type family F a
type instance F Int = Int
data family D a
data instance D Int = DInt
data instance D Float = DFloat

test = [ typeRep ([] :: [T])
       , typeRep ([] :: [F Int])
       , typeRep (Proxy :: Proxy D)
       , typeRep ([] :: [D Int]) ]
