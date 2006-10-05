{-# OPTIONS_GHC -fglasgow-exts #-}

-- This tests scoped type variables, used in an expression 
-- type signature in t1 and t2

module Foo7 where
import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.STRef
import Data.Set hiding (map,filter)

-- a store that allows to mark keys
class Mark m store key | store -> key m where
     new   :: (key,key) -> m store
     mark  :: store -> key -> m ()
     markQ :: store -> key -> m Bool
     seen  :: store -> m [ key ]

-- implementation 1
instance Ord key => Mark (ST s) (STRef s (Set key)) key where
     new   _   = newSTRef empty
     mark  s k = modifySTRef s (insert k)
     markQ s k = liftM (member k) (readSTRef s)
     seen  s   = liftM elems (readSTRef s)

-- implementation 2
instance Ix key => Mark (ST s) (STUArray s key Bool) key where
     new   bnd = newArray bnd False
     mark  s k = writeArray s k True
     markQ     = readArray
     seen  s   = liftM (map fst . filter snd) (getAssocs s)

-- traversing the hull suc^*(start) with loop detection
trav suc start i = new i >>= \ c -> mapM_ (compo c) start >> return c
     where compo c x = markQ c x >>= flip unless (visit c x)
	   visit c x = mark c x >> mapM_ (compo c) (suc x)

-- sample graph
f 1 = 1 : []
f n = n : f (if even n then div n 2 else 3*n+1)

t1 = runST ( (trav f [1..10] (1,52) >>= \ (s::STRef s (Set Int)) -> seen s)
	    :: forall s. ST s [Int] )

t2 = runST ( (trav f [1..10] (1,52) >>= \ (s::STUArray s Int Bool) -> seen s)
	    :: forall s. ST s [Int] )
