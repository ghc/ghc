{-# LANGUAGE TransformListComp, Arrows, MagicHash #-}

-- Tricky cases identified by GlaHiCo

module T27965b where

import GHC.Exts
import Control.Arrow

g1 :: Bool -> Bool
-- Relates to `mkEvalMatchResult`
g1 y = let {-# NOINLINE x #-}
           x = not y
       in let !_ = x in True

g2 xs = [ (x, y) | x <- xs
                , let { {-# NOINLINE y #-}; y = x + 1 }
                , then sortWith by (x + y) ]



g3 :: Arrow a => a (Int, Int) Int
g3 = proc (x, z) ->
       do { let {-# NOINLINE y #-}
                y = x + z
          ; returnA -< y + x }

g4 :: Bool -> Bool
g4 y = let {-# NOINLINE x #-}
           x = not y
       in x `seq` True

g5 :: Arrow a => a Int Int
g5 = proc (I# x) ->
       do  { let {-# NOINLINE y #-}
                 y = x +# 1#
           ; returnA -< I# (y +# x) }
