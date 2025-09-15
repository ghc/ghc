{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}

module T24961 where

import GHC.Types

data R = MkR { a::Int, b::Bool }

-- Tests the that named dropped linear fields are rejected
bad_named :: R %1 -> Int
bad_named (MkR{a=x}) = x

-- Tests that linear unnamed fields in a `{}` pattern are rejected.
data T = MkT Int Bool

bad_anonymous :: T %1 -> ()
bad_anonymous (MkT{}) = ()
