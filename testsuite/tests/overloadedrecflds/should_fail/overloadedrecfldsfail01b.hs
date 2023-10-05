-- Test ambiguous updates are rejected with appropriate error messages

{-# LANGUAGE DuplicateRecordFields #-}

module OverloadedRecFieldsFail1b where

data R = MkR { w :: Bool, x :: Int, y :: Bool }
data S = MkS { w :: Bool, x :: Int, y :: Bool }
data T = MkT { x :: Int, z :: Bool }
data U = MkU { y :: Bool }

-- No type has all these fields
upd2 r = r { x = 3, y = True, z = False }
