-- ghc --make Main.hs -O1; ./Main +RTS -s -RTS

-- The point here is that we want to see a top-level
-- definition like
--
-- lvl_r5ao :: Int
-- lvl_r5ao = case GHC.Real.$wf1 2# 8# of v_B2
--              { __DEFAULT -> GHC.Types.I# v_B2 }
--
-- with the constant (2^8) being floated to top level

{-# LANGUAGE MagicHash #-}

module Main( main ) where

import GHC.Exts

data Attr = Attr !Int  --- the bang is essential

attrFromInt :: Int -> Attr
{-# NOINLINE attrFromInt #-}
attrFromInt w = Attr (w + (2 ^ (8 :: Int)))

fgFromInt :: Int -> Int
{-# INLINE fgFromInt #-}  -- removing this INLINE makes it many times faster
                          -- just like the manually inlined version
                          -- and NOINLINE lands in between
fgFromInt w = w + (2 ^ (8 :: Int))

attrFromIntINLINE :: Int -> Attr
{-# NOINLINE attrFromIntINLINE #-}
attrFromIntINLINE w = Attr (fgFromInt w)

seqFrame2 :: [Int] -> IO ()
{-# NOINLINE seqFrame2 #-}
seqFrame2 l = do
  -- let crux = attrFromInt
  --   Total   time    2.052s  (  2.072s elapsed)
  -- but the following version is many times slower:
  let crux = attrFromIntINLINE
  --   Total   time    7.896s  (  7.929s elapsed)
  mapM_ (\a -> crux a `seq` return ()) l

main :: IO ()
main = seqFrame2 $ replicate 100000000 0
