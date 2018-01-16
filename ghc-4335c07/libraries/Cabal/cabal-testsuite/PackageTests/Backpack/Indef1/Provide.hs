{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Provide where
import Map
newtype MyMap a = MyMap (Map String a)
    deriving (Functor)
