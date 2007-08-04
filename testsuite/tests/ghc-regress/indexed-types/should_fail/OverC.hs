{-# LANGUAGE TypeFamilies #-}

module OverC
where
import OverA (C, D)

data instance C [a] [Int] = C9ListList

type instance D [a] [Int] = Char
