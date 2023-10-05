{-# LANGUAGE TypeFamilies #-}
-- Tests that we check family instance consistency between
-- type family instances defined in the currently compiled module
-- and the transitive imports.
module OverIndirectThisModD
where
import OverIndirectThisModC
  -- imports OverIndirectThisModB with conflicting instances
import OverIndirectThisModA (C, D)

data instance C [a] [Int] = C9ListList

type instance D [a] [Int] = Char
