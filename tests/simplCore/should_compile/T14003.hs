{-# OPTIONS_GHC -fspec-constr -fmax-worker-args=2 #-}

-- | Ensure that functions with SPEC arguments are constructor-specialised
-- even if their argument count exceeds -fmax-worker-args.
module T14003 (pat1, pat2, pat3, pat4) where

import GHC.Exts

hi :: SPEC
   -> Maybe Int
   -> Maybe Int
   -> Maybe Int
   -> Int
hi SPEC (Just x) (Just y) (Just z) = x+y+z
hi SPEC (Just x) _        _        = hi SPEC (Just x) (Just 42) Nothing
hi SPEC Nothing  _        _        = 42

pat1 :: Int -> Int
pat1 n = hi SPEC (Just n) (Just 4) (Just 0)

pat2 :: Int -> Int
pat2 n = hi SPEC Nothing (Just n) Nothing

pat3 :: Int -> Int
pat3 n = hi SPEC Nothing Nothing (Just n)

pat4 :: Int -> Int
pat4 n = hi SPEC Nothing (Just n) (Just n)


