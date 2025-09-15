{-# LANGUAGE PolyKinds,
             GADTs,
             DataKinds,
             KindSignatures
 #-}

module T5935 where

data SList a where
  SNil :: SList '[]

x :: SList ('[] :: [Bool])
x = SNil
