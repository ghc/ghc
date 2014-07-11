{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}

module T6018failclosed7 where

type family FClosed a b c = (result :: *) | result -> a b c where
    FClosed Int  Char Bool = Bool
    FClosed Char Bool Int  = Int
    FClosed Bool Int  Char = Int
