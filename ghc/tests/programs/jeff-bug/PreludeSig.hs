module PreludeSig where

import Prelude(Ord,Bool,Int,($),(.))
import qualified Prelude as P
import qualified List
import Monad
import Signal

-- Begin Signature ----------------------------------------------------
{- 
   Functions in this module are functions from the Haskell
   prelude lifted on Signals
-}

last ::  Signal [a] -> Signal a
head :: Signal [a] -> Signal a
tail :: Signal [a] -> Signal [a]
splitAt :: Signal Int -> Signal [a] -> (Signal [a], Signal [a])
length :: Signal [a] -> Signal Int
max :: Ord a => Signal a -> Signal a -> Signal a
min :: Ord a => Signal a -> Signal a -> Signal a
maximum :: Ord a => Signal [a] -> Signal a
minimum :: Ord a => Signal [a] -> Signal a
filter :: MonadPlus c => (a -> Bool) -> Signal (c a) -> Signal (c a)
partition :: (a -> Bool) -> Signal [a] -> (Signal [a],Signal [a])
fst :: Signal (a,b) -> Signal a
snd :: Signal (a,b) -> Signal b

-- End Signature -----------------------------------------------------
last = lift1 P.last

head = lift1 P.head

tail = lift1 P.tail

splitAt x y = unbundle2 $ lift2 P.splitAt x y

length = lift1 P.length

max = lift2 P.max

min = lift2 P.min

maximum = lift1 P.maximum

minimum = lift1 P.minimum

filter p y = lift1 filt y
	   where
	     filt m = do { x <- m;
			   if p x then mzero else return x }

partition x y = unbundle2 (lift1 (List.partition x) y )

fst = lift1 P.fst

snd = lift1 P.snd

-- Begin Signature ----------------------------------------------------
