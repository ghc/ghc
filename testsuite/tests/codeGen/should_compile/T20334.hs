{-# LANGUAGE MagicHash #-}
module T20334 (V (..), W (..), g, h, k) where

import GHC.Exts

data V = V Double# Double# Double#
data W = W [Int] !V

-- Opaque, so that the workers keep v boxed.
{-# OPAQUE useBoxed #-}
useBoxed :: V -> Double#
useBoxed (V a _ _) = a

{-# NOINLINE useFields #-}
useFields :: Double# -> Double# -> Double# -> Double#
useFields a b c = a +## b +## c

-- The worker of each binds v's fields where it starts, v coming from a strict
-- field.  The fields are used only after a call, the evaluation of x in g and
-- length in h, and v is used whole on another path, so that v is saved across
-- the call anyway: the fields are loaded from it after the call, not saved
-- beside it and reloaded from the stack.
{-# NOINLINE g #-}
g :: W -> Double#
g (W osh v) = case osh of
  [] -> 0.0##
  x : _ -> if x == 0 then useBoxed v else case v of V a b c -> useFields a b c

{-# NOINLINE h #-}
h :: Int -> W -> Double#
h d (W s v)
  | d >= length s = useBoxed v
  | otherwise = case v of V a b c -> useFields a b c

-- As h, but v is not used whole, so that it is dead across the calls where all
-- its fields are live: v is saved in their place, and the fields are loaded
-- from it after the calls.  Opaque, so that no worker takes the fields in
-- place of v.
{-# OPAQUE k #-}
k :: Int -> W -> Double#
k d (W s v) = case v of
  V a b c -> if d >= length s then 0.0## else useFields a b c
