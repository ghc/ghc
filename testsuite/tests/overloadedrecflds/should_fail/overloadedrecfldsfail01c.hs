-- Test ambiguous updates are rejected with appropriate error messages

{-# LANGUAGE DuplicateRecordFields #-}

module OverloadedRecFieldsFail1c where

data R = MkR { w :: Bool, x :: Int, y :: Bool }
data S = MkS { w :: Bool, x :: Int, y :: Bool }
data T = MkT { x :: Int, z :: Bool }
data U = MkU { y :: Bool }

-- User-specified type does not have these fields
upd3 r = r { w = True, x = 3, y = True } :: U
