{-# LANGUAGE MagicHash #-}
module T23938 where

import T23938A
import Control.Monad.ST

genIndexes :: () -> ST RealWorld (GVector RealWorld (T Int))
genIndexes = new f
