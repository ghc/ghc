{-# LANGUAGE StandaloneKindSignatures #-}

module T16727a where

type T1 :: T2
data T1

type T2 :: T1
data T2
