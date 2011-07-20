{-# OPTIONS -XRecursiveDo #-}

module Main(main) where

import Control.Monad.Fix
import Control.Monad.ST
import Data.STRef

newtype Node s a = N (STRef s Bool, Node s a, a, Node s a)

newNode       :: Node s a -> a -> Node s a -> ST s (Node s a)
newNode b c f = do v <- newSTRef False
                   return (N (v, b, c, f))

ll :: ST s (Node s Int)
ll = mdo n0 <- newNode n3 0 n1
         n1 <- newNode n0 1 n2
         n2 <- newNode n1 2 n3
         n3 <- newNode n2 3 n0
         return n0

data Direction = Forward | Backward deriving Eq

traverse                      :: Direction -> Node s a -> ST s [a]
traverse dir (N (v, b, i, f)) = 
       do visited <- readSTRef v
          if visited
             then return []
             else do writeSTRef v True
                     let n = if dir == Forward then f else b
                     is <- traverse dir n
                     return (i:is)

l2dll        :: [a] -> ST s (Node s a)
l2dll (x:xs) = mdo c <- newNode l x f
                   (f, l) <- l2dll' c xs
                   return c

l2dll'          :: Node s a -> [a] -> ST s (Node s a, Node s a)
l2dll' p []     = return (p, p)
l2dll' p (x:xs) = mdo c <- newNode p x f
                      (f, l) <- l2dll' c xs
                      return (c, l)

insertAfter :: Node s a -> a -> ST s (Node s a)
insertAfter cur@(N (v, prev, val, next)) i 
     = do vis <- newSTRef False
          let newCell = N (vis, cur, i, next)
	  return (N (v, prev, val, newCell))

test = runST (do l   <- l2dll [1 .. 10] 
                 l'  <- insertAfter l  12 
		 l'' <- insertAfter l' 13 
		 traverse Forward l'')

main = print test
