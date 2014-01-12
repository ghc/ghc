module Thread 
( ThreadTree (..)
, ContM (..)
, atom
, stop
, buildThread
)
where

----------------------------------
data ThreadTree req rsp m = 
        Atom (m (ThreadTree req rsp m))
      | Stop
----------------------------------
newtype ContM req rsp m a = ContM ((a-> ThreadTree req rsp m)-> ThreadTree req rsp m)

instance Monad m => Monad (ContM req rsp m) where
   m >>= f = contmBind m f
   return  = contmReturn

contmBind :: (ContM req rsp m a) -> (a -> (ContM req rsp m b)) -> (ContM req rsp m b)
contmBind (ContM x) f =  
    ContM(\y-> x (\z-> let ContM f' = f z in f' y))
contmReturn :: a -> (ContM req rsp m a)
contmReturn x = ContM(\c -> c x)

{-- how to build primitive ContM blocks... --}

atom :: Monad m => (m a) -> (ContM req rsp m a)
atom mx = ContM( \c -> Atom( do x <- mx; return (c x) ))

stop :: (ContM req rsp m a)
stop = ContM( \c -> Stop )

buildThread :: (ContM req rsp m a) -> ThreadTree req rsp m
buildThread (ContM f) = f (\c->Stop)

----------------------------------
