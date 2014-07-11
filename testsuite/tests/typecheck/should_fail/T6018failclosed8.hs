{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}

module T6018failclosed8 where

type family IClosed a b c = r | r -> a b where
    IClosed Int  Char Bool = Bool
    IClosed Int  Int  Int  = Bool
    IClosed Bool Int  Int  = Int
