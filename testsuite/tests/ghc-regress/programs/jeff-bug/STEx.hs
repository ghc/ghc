module STEx where
import LazyST
import Monad

infixl 1 `handle`

-- Begin Signature ----------------------------------------------------------

{-
  STEx synthesizes the state and exception monads.  
-}
 

{-data STEx s a-}

{-instance Monad (STEx s)-}
{-instance MonadPlus (STEx s)-}

-- c `handle` x, return x if c raises an exception
handle :: STEx a b -> b -> ST a b

-- lift an exception or st monad thing or into STEx
liftEx :: Maybe a -> STEx s a
liftST :: ST s a -> STEx s a

-- raise an exception if Bool is False
assert :: Bool -> STEx s ()

-- the following functions have the same meaning as their corresponding
-- state monad functions
{-readVarSTEx :: MutVar a b -> STEx a b-}
{-writeVarSTEx :: MutVar a b -> b -> STEx a ()-}
{-newVarSTEx :: a -> STEx b (MutVar b a)-}

{-readArraySTEx :: Ix b => MutArr a b c -> b -> STEx a c-}
{-writeArraySTEx :: Ix b => MutArr a b c -> b -> c -> STEx a ()-}
{-newArraySTEx :: Ix a => (a,a) -> b -> STEx c (MutArr c a b)-}

-- End Signature -----------------------------------------------------------

newtype STEx s a = STEx (ST s (Maybe a))

instance Monad (STEx s) where
	return = STEx . return . return
	(STEx x) >>= f 
             = STEx $ do y <- x
                         case y of 
	                    Just z -> let STEx z' = f z 
                                      in z'
                            Nothing -> return Nothing

instance MonadPlus (STEx s) where
	mzero = liftEx mzero
	(STEx x) `mplus` (STEx y) = STEx $ do x' <- x
                        	              y' <- y
                                   	      return $ mplus x' y'
liftST x = STEx $ do {z <- x ; return $ return z}

liftEx x = STEx $ return x


handle (STEx m) x 
    = do y <- m 
         case y of 
	    Just z -> return z
            Nothing -> return x

readVarSTEx v    = liftST $ readSTRef v
writeVarSTEx v x = liftST $ writeSTRef v x
newVarSTEx x     = liftST $ newSTRef x

readArraySTEx v n    = liftST $ readSTArray v n
writeArraySTEx v x n = liftST $ writeSTArray v x n
newArraySTEx x n     = liftST $ newSTArray x n

{- example 
f x = do y <- liftEx x
         v <- newVarSTEx y
         readVarSTEx v

g x = runST (handle (f x) 2)
-}

assert True = liftEx $ Just ()
assert False = liftEx $ Nothing

