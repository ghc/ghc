{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module T15499 ()
where

data ADT (p :: Integer) where
  ADT ::
    { a :: a
    , b :: Integer
    } -> ADT p

foo = undefined {b=undefined}
