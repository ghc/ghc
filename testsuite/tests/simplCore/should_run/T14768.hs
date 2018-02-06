{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Control.Monad (forM_, liftM)
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Exts

vec :: U.Vector Moebius
vec = U.singleton Moebius0

main :: IO ()
main = print $ U.head vec == U.head vec

data Moebius = Moebius0 | Moebius1 | Moebius2
  deriving (Eq)

fromMoebius :: Moebius -> Int
fromMoebius Moebius0 = 0
fromMoebius Moebius1 = 1
fromMoebius Moebius2 = 2

toMoebius :: Int -> Moebius
toMoebius (I# i#) = tagToEnum# i#

newtype instance U.MVector s Moebius = MV_Moebius (P.MVector s Int)
newtype instance U.Vector    Moebius = V_Moebius  (P.Vector    Int)

instance U.Unbox Moebius

instance M.MVector U.MVector Moebius where
  basicLength (MV_Moebius v) = M.basicLength v
  basicUnsafeSlice i n (MV_Moebius v) = MV_Moebius $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Moebius v1) (MV_Moebius v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Moebius `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Moebius v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_Moebius `liftM` M.basicUnsafeReplicate n (fromMoebius x)
  basicUnsafeRead (MV_Moebius v) i = toMoebius `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Moebius v) i x = M.basicUnsafeWrite v i (fromMoebius x)
  basicClear (MV_Moebius v) = M.basicClear v
  basicSet (MV_Moebius v) x = M.basicSet v (fromMoebius x)
  basicUnsafeCopy (MV_Moebius v1) (MV_Moebius v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Moebius v1) (MV_Moebius v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Moebius v) n = MV_Moebius `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Moebius where
  basicUnsafeFreeze (MV_Moebius v) = V_Moebius `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Moebius v) = MV_Moebius `liftM` G.basicUnsafeThaw v
  basicLength (V_Moebius v) = G.basicLength v
  basicUnsafeSlice i n (V_Moebius v) = V_Moebius $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Moebius v) i = toMoebius `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Moebius mv) (V_Moebius v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
