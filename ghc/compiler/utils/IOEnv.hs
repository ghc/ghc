-- (c) The University of Glasgow 2002
--
-- The IO Monad with an environment
--

module IOEnv (
	IOEnv,	-- Instance of Monad

	-- Standard combinators, specialised
	returnM, thenM, thenM_, failM,
	mappM, mappM_, mapSndM, sequenceM, foldlM, 
	mapAndUnzipM, mapAndUnzip3M, 
	checkM, ifM, zipWithM, zipWithM_,

	-- Getting at the environment
	getEnv, setEnv, updEnv,

	runIOEnv, unsafeInterleaveM,			
	tryM, fixM, 

	-- I/O operations
	ioToIOEnv,
	IORef, newMutVar, readMutVar, writeMutVar, updMutVar
  ) where
#include "HsVersions.h"

import Panic		( tryJust )
import DATA_IOREF	( IORef, newIORef, readIORef, writeIORef )
import UNSAFE_IO	( unsafeInterleaveIO )
import FIX_IO		( fixIO )
import EXCEPTION	( Exception(..) )
import IO		( isUserError )


----------------------------------------------------------------------
--		Defining the monad type
----------------------------------------------------------------------


newtype IOEnv env a = IOEnv (env -> IO a)
unIOEnv (IOEnv m) = m

instance Monad (IOEnv m) where
  (>>=)  = thenM
  (>>)   = thenM_
  return = returnM
  fail s = failM	-- Ignore the string

returnM :: a -> IOEnv env a
returnM a = IOEnv (\ env -> return a)

thenM :: IOEnv env a -> (a -> IOEnv env b) -> IOEnv env b
thenM (IOEnv m) f = IOEnv (\ env -> do { r <- m env ;
				       unIOEnv (f r) env })

thenM_ :: IOEnv env a -> IOEnv env b -> IOEnv env b
thenM_ (IOEnv m) f = IOEnv (\ env -> do { m env ; unIOEnv f env })

failM :: IOEnv env a
failM = IOEnv (\ env -> ioError (userError "IOEnv failure"))



----------------------------------------------------------------------
--	Fundmantal combinators specific to the monad
----------------------------------------------------------------------


---------------------------
runIOEnv :: env -> IOEnv env a -> IO a
runIOEnv env (IOEnv m) = m env


---------------------------
{-# NOINLINE fixM #-}
  -- Aargh!  Not inlining fixTc alleviates a space leak problem.
  -- Normally fixTc is used with a lazy tuple match: if the optimiser is
  -- shown the definition of fixTc, it occasionally transforms the code
  -- in such a way that the code generator doesn't spot the selector
  -- thunks.  Sigh.

fixM :: (a -> IOEnv env a) -> IOEnv env a
fixM f = IOEnv (\ env -> fixIO (\ r -> unIOEnv (f r) env))


---------------------------
tryM :: IOEnv env r -> IOEnv env (Either Exception r)
-- Reflect exception into IOEnv envonad
tryM (IOEnv thing) = IOEnv (\ env -> tryJust tc_errors (thing env))
  where 
#if __GLASGOW_HASKELL__ > 504 || __GLASGOW_HASKELL__ < 500
	tc_errors e@(IOException ioe) | isUserError ioe = Just e
#elif __GLASGOW_HASKELL__ == 502
	tc_errors e@(UserError _) = Just e
#else 
	tc_errors e@(IOException ioe) | isUserError e = Just e
#endif
	tc_errors _other = Nothing
	-- type checker failures show up as UserErrors only


---------------------------
unsafeInterleaveM :: IOEnv env a -> IOEnv env a
unsafeInterleaveM (IOEnv m) = IOEnv (\ env -> unsafeInterleaveIO (m env))


----------------------------------------------------------------------
--	Accessing input/output
----------------------------------------------------------------------

ioToIOEnv :: IO a -> IOEnv env a
ioToIOEnv io = IOEnv (\ env -> io)

newMutVar :: a -> IOEnv env (IORef a)
newMutVar val = IOEnv (\ env -> newIORef val)

writeMutVar :: IORef a -> a -> IOEnv env ()
writeMutVar var val = IOEnv (\ env -> writeIORef var val)

readMutVar :: IORef a -> IOEnv env a
readMutVar var = IOEnv (\ env -> readIORef var)

updMutVar :: IORef a -> (a->a) -> IOEnv env ()
updMutVar var upd_fn = IOEnv (\ env -> do { v <- readIORef var; writeIORef var (upd_fn v) })


----------------------------------------------------------------------
--	Accessing the environment
----------------------------------------------------------------------

getEnv :: IOEnv env env
{-# INLINE getEnv #-}
getEnv = IOEnv (\ env -> return env)

setEnv :: env' -> IOEnv env' a -> IOEnv env a
{-# INLINE setEnv #-}
setEnv new_env (IOEnv m) = IOEnv (\ env -> m new_env)

updEnv :: (env -> env') -> IOEnv env' a -> IOEnv env a
{-# INLINE updEnv #-}
updEnv upd (IOEnv m) = IOEnv (\ env -> m (upd env))


----------------------------------------------------------------------
--	Standard combinators, but specialised for this monad
--			(for efficiency)
----------------------------------------------------------------------

mappM  	      :: (a -> IOEnv env b) -> [a] -> IOEnv env [b]
mappM_ 	      :: (a -> IOEnv env b) -> [a] -> IOEnv env ()
mapSndM       :: (b -> IOEnv env c) -> [(a,b)] -> IOEnv env [(a,c)]
	-- Funny names to avoid clash with Prelude
sequenceM     :: [IOEnv env a] -> IOEnv env [a]
foldlM        :: (a -> b -> IOEnv env a)  -> a -> [b] -> IOEnv env a
mapAndUnzipM  :: (a -> IOEnv env (b,c))   -> [a] -> IOEnv env ([b],[c])
mapAndUnzip3M :: (a -> IOEnv env (b,c,d)) -> [a] -> IOEnv env ([b],[c],[d])
checkM	      :: Bool -> IOEnv env () -> IOEnv env ()	-- Perform arg if bool is False
ifM	      :: Bool -> IOEnv env () -> IOEnv env ()	-- Perform arg if bool is True

mappM f []     = return []
mappM f (x:xs) = do { r <- f x; rs <- mappM f xs; return (r:rs) }

mapSndM f []     = return []
mapSndM f ((a,b):xs) = do { c <- f b; rs <- mapSndM f xs; return ((a,c):rs) }

mappM_ f []     = return ()
mappM_ f (x:xs) = f x >> mappM_ f xs

zipWithM :: (a -> b -> IOEnv env c) -> [a] -> [b] -> IOEnv env [c]
zipWithM f [] bs = return []
zipWithM f as [] = return []
zipWithM f (a:as) (b:bs) = do { r <- f a b; rs <- zipWithM f as bs; return (r:rs) } 

zipWithM_ :: (a -> b -> IOEnv env c) -> [a] -> [b] -> IOEnv env ()
zipWithM_ f [] bs = return ()
zipWithM_ f as [] = return ()
zipWithM_ f (a:as) (b:bs) = do { f a b; zipWithM_ f as bs } 

sequenceM [] = return []
sequenceM (x:xs) = do { r <- x; rs <- sequenceM xs; return (r:rs) }

foldlM k z [] = return z
foldlM k z (x:xs) = do { r <- k z x; foldlM k r xs }

mapAndUnzipM f []     = return ([],[])
mapAndUnzipM f (x:xs) = do { (r,s) <- f x; 
			     (rs,ss) <- mapAndUnzipM f xs; 
			     return (r:rs, s:ss) }

mapAndUnzip3M f []     = return ([],[], [])
mapAndUnzip3M f (x:xs) = do { (r,s,t) <- f x; 
			      (rs,ss,ts) <- mapAndUnzip3M f xs; 
			      return (r:rs, s:ss, t:ts) }

checkM True  err = return ()
checkM False err = err

ifM True  do_it = do_it
ifM False do_it = return ()
