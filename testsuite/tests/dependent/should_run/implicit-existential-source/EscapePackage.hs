{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

module EscapePackage where

import Filter (Hidden, toList)
import Functions (Function)
import Vector

hideElement :: Vec n a -> Hidden (Vec n a)
hideElement xs = Cons xs Nil

bad :: Function a -> a -> Int
bad builder x =
    let packed = hideElement (builder x)
    in length (toList packed)
