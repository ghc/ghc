-- Test ambiguous updates are rejected with appropriate error messages

{-# LANGUAGE DuplicateRecordFields #-}

module OverloadedRecFieldsFail1a where

data R = MkR { w :: Bool, x :: Int, y :: Bool }
data S = MkS { w :: Bool, x :: Int, y :: Bool }
data T = MkT { x :: Int, z :: Bool }
data U = MkU { y :: Bool }

-- Straightforward ambiguous update
upd1 r = r { x = 3 }
