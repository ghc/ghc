{-# LANGUAGE DuplicateRecordFields #-}
module DRFHoleFits where
import qualified DRFHoleFits_A as A

data T = MkT { foo :: Int }

bar = _ :: T -> Int
baz = _ :: A.S -> Int
