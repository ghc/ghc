{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeApplications #-}

module T15245 where

import Type.Reflection

data family K
data instance K = MkK

main = print (typeRep @'MkK)
