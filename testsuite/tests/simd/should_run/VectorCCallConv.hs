{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import Data.Int
import GHC.Int
import GHC.Prim

foreign import ccall "printVecs_int64x2_c"
  printVecs_int64x2# ::
    Int64X2# -> -- v1
    Int64X2# -> -- v2
    Int64X2# -> -- v3
    Int64X2# -> -- v4
    Int64X2# -> -- v5
    Int64X2# -> -- v6
    Int64X2# -> -- v7
    Int64X2# -> -- v8
    Int64X2# -> -- v9
    Int64X2# -> -- v10
    Int64X2# -> -- v11
    Int64X2# -> -- v12
    Int64X2# -> -- v13
    Int64X2# -> -- v14
    Int64X2# -> -- v15
    Int64X2# -> -- v16
    Int64X2# -> -- v17
    Int64X2# -> -- v18
    Int64X2# -> -- v19
    Int64X2# -> -- v20
    Int64X2# -> -- v21
    Int64X2# -> -- v22
    Int64X2# -> -- v23
    Int64X2# -> -- v24
    Int64X2# -> -- v25
    --    Int64X2# -> -- v26
    --    Int64X2# -> -- v27
    --    Int64X2# -> -- v28
    --    Int64X2# -> -- v29
    --    Int64X2# -> -- v30
    --    Int64X2# ->
    --    Int64X2# ->
    --    Int64X2# ->
    --    Int64X2# ->
    --    Int64X2# ->
    --    Int64X2# ->
    IO ()

-- foreign import ccall "return_int64X2"
--   return_int64X2# :: (# #) -> Int64X2#
--
-- unpackInt64X2 :: Int64X2# -> (Int64, Int64)
-- unpackInt64X2 v = case unpackInt64X2# v of
--   (# x0, x1 #) -> (I64# x0, I64# x1)

main :: IO ()
main = do
  let v1 = packInt64X2# (# 0#Int64, 1#Int64 #)
      v2 = packInt64X2# (# 2#Int64, 3#Int64 #)
      v3 = packInt64X2# (# 4#Int64, 5#Int64 #)
      v4 = packInt64X2# (# 6#Int64, 7#Int64 #)
      v5 = packInt64X2# (# 8#Int64, 9#Int64 #)
      v6 = packInt64X2# (# 10#Int64, 11#Int64 #)
      v7 = packInt64X2# (# 12#Int64, 13#Int64 #)
      v8 = packInt64X2# (# 14#Int64, 15#Int64 #)
      v9 = packInt64X2# (# 16#Int64, 17#Int64 #)
      v10 = packInt64X2# (# 18#Int64, 19#Int64 #)
      v11 = packInt64X2# (# 20#Int64, 21#Int64 #)
      v12 = packInt64X2# (# 22#Int64, 23#Int64 #)
      v13 = packInt64X2# (# 24#Int64, 25#Int64 #)
      v14 = packInt64X2# (# 26#Int64, 27#Int64 #)
      v15 = packInt64X2# (# 28#Int64, 29#Int64 #)
      v16 = packInt64X2# (# 30#Int64, 31#Int64 #)
      v17 = packInt64X2# (# 32#Int64, 33#Int64 #)
      v18 = packInt64X2# (# 34#Int64, 35#Int64 #)
      v19 = packInt64X2# (# 36#Int64, 37#Int64 #)
      v20 = packInt64X2# (# 38#Int64, 39#Int64 #)
      v21 = packInt64X2# (# 40#Int64, 41#Int64 #)
      v22 = packInt64X2# (# 42#Int64, 43#Int64 #)
      v23 = packInt64X2# (# 44#Int64, 45#Int64 #)
      v24 = packInt64X2# (# 46#Int64, 47#Int64 #)
      v25 = packInt64X2# (# 48#Int64, 49#Int64 #)
      v26 = packInt64X2# (# 50#Int64, 51#Int64 #)
      v27 = packInt64X2# (# 52#Int64, 53#Int64 #)
      v28 = packInt64X2# (# 54#Int64, 55#Int64 #)
      v29 = packInt64X2# (# 56#Int64, 57#Int64 #)
      v30 = packInt64X2# (# 58#Int64, 59#Int64 #)
  --      v31 = packInt64X2# (# 60#Int64, 61#Int64 #)
  --      v32 = packInt64X2# (# 62#Int64, 63#Int64 #)
  --      v33 = packInt64X2# (# 64#Int64, 65#Int64 #)
  --      v34 = packInt64X2# (# 66#Int64, 67#Int64 #)
  --      v35 = packInt64X2# (# 68#Int64, 69#Int64 #)
  --      v36 = packInt64X2# (# 70#Int64, 71#Int64 #)

  printVecs_int64x2#
    v1
    v2
    v3
    v4
    v5
    v6
    v7
    v8
    v9
    v10
    v11
    v12
    v13
    v14
    v15
    v16
    v17
    v18
    v19
    v20
    v21
    v22
    v23
    v24
    v25

--    v26

--    v27
--    v28
--    v29
--    v30

--    v31
--    v32
--    v33
--    v34
--    v35
--    v26

--    let v = return_int64X2#
--    print $ unpackInt64X2 v
