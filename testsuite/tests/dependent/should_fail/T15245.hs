{-# LANGUAGE TypeFamilies, DataKinds #-}

module T15245 where

import Type.Reflection

data family K
data instance K = MkK

main = print (typeRep @'MkK)
