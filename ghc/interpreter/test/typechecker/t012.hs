--!!! runST (the classic rank 2 type example)

newtype ST s a = MkST (s -> (a,s))

unST :: ST s a -> (s -> (a,s))
unST (MkST f) = f

runST :: (forall s. ST s a) -> a
runST m = case unST m () of { (a,_)  -> 
          a
	  }

returnST :: a -> ST s a
returnST a = MkST (\s -> (a,s))

thenST :: ST s a -> (a -> ST s b) -> ST s b
thenST m k = MkST (\ s0 -> case unST m s0 of { (a,s1) -> unST (k a) s1 })

instance Monad (ST s) where
    return = returnST
    (>>=)  = thenST

