{-# LANGUAGE BangPatterns #-}

import Data.Array.Base
import GHC.Arr(unsafeIndex,index)
import Data.Array.IArray
import Control.Monad.ST
import Data.Array.ST
import System.Environment(getArgs)

type Elem = Double
type Vector = [Elem]
type Matrix = [Vector]

n :: Num a => a
n = 40

a :: Matrix
a = [[if i==j then 1 else 0|i<-[1..n]]|j<-[1..n]]

p :: Vector
p = [1..n]

------------------------ array-based, update-in-place code

type VectorA s = STUArray s Int Elem
type MatrixA s = STUArray s (Int,Int) Elem

{-# INLINE myreadArray #-}
-- | Read an element from a mutable array
myreadArray :: (MArray a e m, Ix i) => a i e -> i -> m e
myreadArray marr i = do
  (l,u) <- getBounds marr
  unsafeRead marr (myindex (l,u) i)

{-# INLINE mywriteArray #-}
-- | Write an element in a mutable array
mywriteArray :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()
mywriteArray marr i e = do
  (l,u) <- getBounds marr
  unsafeWrite marr (myindex (l,u) i) e

myindex b i = index b i
-- the following is supposed to be the default implementation of index,
-- from GHC.Arr
-- myindex b i | inRange b i = unsafeIndex b i
--             | otherwise   = error "Error in array index"

matA :: MatrixA s -> VectorA s -> VectorA s -> ST s (VectorA s)
(m  `matA` v) tmp = m `seq` v `seq` tmp `seq` l 1 1 0
  where l !i !j !s | i>n = return tmp
        l  i  j  s | j>n = mywriteArray tmp i s >> l (i+1) 1 0
        l  i  j  s       = do a<-myreadArray m (i,j)
                              b<-myreadArray v j
                              l i (j+1) (s+a*b)

loopA a p q n | n==0 = return q
loopA a p q n        = do
  (a `matA` p) q
  loopA a p q (n-1)

testA c = runSTUArray (do
  aA <- newListArray ((1,1),(n,n)) (concat a)
  pA <- newListArray (1,n) p
  qA <- newArray (1,n) 0
  loopA aA pA qA c
  )

main = print $ testA 100_000
