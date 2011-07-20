-- !! Test import-unused warnings on 'module M' exports
-- This one should not generate any warnings

module M (module Mod171_A, h) where

import Mod171_A	-- This isn't unused...
import Mod171_B	-- even though this imports all the same stuff

h :: Int -> Int
h = g

