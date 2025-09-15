{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NondecreasingIndentation #-}

import GHC.Exts
import Debug.Trace
import Control.Exception

expensive :: Int -> Int
expensive n = trace ("expensive " ++ show n) n
{-# OPAQUE expensive #-}

expensive# :: Int -> Int#
expensive# (I# n) = trace ("expensive# " ++ show (I# n) ++ "#") () `seq` n
{-# OPAQUE expensive# #-}

f :: Int# -> Int -> Int -> Int
f _ _ n = n -- arity 3
{-# OPAQUE f #-}

data T where
  K1 :: !Int -> Int -> T  -- strict field
  K2 :: Int# -> Int -> T  -- unlifted field

main = do
  -- The idea is that every `seq` encodes an `exprIsHNF` check,
  -- and that every `evaluate` encodes an `exprOkForDiscard` check.
  -- When the expression *is not* a value/ok-for-discard, we must see a trace
  -- message.
  -- For the expressions considered, both checks should agree.
  -- Furthermore, a specification for these examples is given in
  -- Note [exprIsHNF for function applications].
  evaluate $ f (expensive# 1) 1         -- not HNF
  evaluate $ f 2# (expensive 2)         -- HNF
  evaluate $ K1 1 (expensive 3)         -- HNF
  evaluate $ K1 (expensive 4) 2         -- Not HNF
  evaluate $ K1 (expensive 5)           -- HNF
  evaluate $ K2 1# (expensive 6)        -- HNF
  evaluate $ K2 (expensive# 7) 2        -- Not HNF
  evaluate $ K2 (expensive# 8)          -- Not HNF
  f (expensive# 11) 1  `seq` return ()  -- not HNF
  f 2# (expensive 12)  `seq` return ()  -- HNF
  K1 1 (expensive 13)  `seq` return ()  -- HNF
  K1 (expensive 14) 2  `seq` return ()  -- Not HNF
  K1 (expensive 15)    `seq` return ()  -- HNF
  K2 1# (expensive 16) `seq` return ()  -- HNF
  K2 (expensive# 17) 2 `seq` return ()  -- Not HNF
  K2 (expensive# 18)   `seq` return ()  -- Not HNF
