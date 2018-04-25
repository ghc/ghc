{-# LANGUAGE TypeFamilies #-}
-- Tests that we check family instance consistency between
-- type family instances defined in the currently compiled module
-- and the direct imports.
module OverDirectThisModC
where
import OverDirectThisModB
import OverDirectThisModA (C, D)

data instance C [a] [Int] = C9ListList

type instance D [a] [Int] = Char
