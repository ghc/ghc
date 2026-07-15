{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Exts
import GHC.Exts.Heap (GenClosure(..), getClosureData)
import System.Exit

data Box a = Box !a

update :: SmallArray# a -> Int# -> a -> SmallArray# a
update ary i# x = runRW# $ \s0 ->
  case thawSmallArray# ary 0# (sizeofSmallArray# ary) s0 of
    (# s1, mary #) -> case writeSmallArray# mary i# x s1 of
      s2 -> case unsafeFreezeSmallArray# mary s2 of
        (# _, ary' #) -> ary'
{-# INLINE update #-}

good :: Bool -> a -> SmallArray# (Box a) -> SmallArray# (Box a)
good b !x ary =
  let exit y i# = update ary i# (Box y)
  in if b then exit x 0# else exit x 1#
{-# NOINLINE good #-}

recursive :: Bool -> a -> SmallArray# (Box a) -> SmallArray# (Box a)
recursive b !x ary =
  let exit y i# =
        case i# ==# 0# of
          1# -> update ary 0# (Box y)
          _  -> exit y (i# -# 1#)
  in if b then exit x 0# else exit x 1#
{-# NOINLINE recursive #-}

mixed :: Bool -> a -> a -> SmallArray# (Box a) -> SmallArray# (Box a)
mixed b !x z ary =
  let exit y i# = update ary i# (Box y)
  in if b then exit x 0# else exit z 1#
{-# NOINLINE mixed #-}

mkArr :: a -> SmallArray# a
mkArr x = runRW# $ \s0 ->
  case newSmallArray# 2# x s0 of
    (# s1, mary #) -> case unsafeFreezeSmallArray# mary s1 of
      (# _, ary #) -> ary

checkConstr :: String -> SmallArray# (Box Int) -> Int# -> IO ()
checkConstr label ary i# =
  case indexSmallArray# ary i# of
    (# value #) -> do
      closure <- getClosureData value
      case closure of
        ConstrClosure{} -> pure ()
        _ -> putStrLn ("FAIL: " ++ label ++ " was not a constructor") >> exitFailure

checkThunk :: String -> SmallArray# (Box Int) -> Int# -> IO ()
checkThunk label ary i# =
  case indexSmallArray# ary i# of
    (# value #) -> do
      closure <- getClosureData value
      case closure of
        ThunkClosure{} -> pure ()
        _ -> putStrLn ("FAIL: " ++ label ++ " was not a thunk") >> exitFailure

main :: IO ()
main = do
  let initial = mkArr (Box (0 :: Int))
  checkConstr "non-recursive join" (good True 42 initial) 0#
  checkConstr "recursive join" (recursive False 42 initial) 0#
  checkThunk "mixed incoming tags" (mixed True 42 undefined initial) 0#
  putStrLn "OK"
