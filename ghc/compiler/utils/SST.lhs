\section{SST: the strict state transformer monad}
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
#include "HsVersions.h"

module SST(
	SYN_IE(SST), SST_R, SYN_IE(FSST), FSST_R,

	runSST, sstToST, stToSST,
	thenSST, thenSST_, returnSST, fixSST,
	thenFSST, thenFSST_, returnFSST, failFSST,
	recoverFSST, recoverSST, fixFSST,
	unsafeInterleaveSST, 

	newMutVarSST, readMutVarSST, writeMutVarSST
#if __GLASGOW_HASKELL__ >= 200
	, MutableVar
#else
	, MutableVar(..), _MutableArray
#endif
  ) where

#if __GLASGOW_HASKELL__ >= 200
import GHCbase
#else
import PreludeGlaST ( MutableVar(..), _MutableArray(..), ST(..) )
#endif

CHK_Ubiq() -- debugging consistency check
\end{code}

\begin{code}
data SST_R s r = SST_R r (State# s)
type SST s r = State# s -> SST_R s r

\end{code}

\begin{code}
-- converting to/from ST

sstToST :: SST s r -> ST s r
stToSST :: ST s r -> SST s r

#if __GLASGOW_HASKELL__ >= 200

sstToST sst = ST $ \ (S# s) ->
   case sst s of SST_R r s' -> (r, S# s')

stToSST (ST st) = \ s ->
   case st (S# s) of (r, S# s') -> SST_R r s'

#else
sstToST sst (S# s)
  = case sst s of SST_R r s' -> (r, S# s')
stToSST st s
  = case st (S# s) of (r, S# s') -> SST_R r s'
#endif

-- Type of runSST should be builtin ...
-- runSST :: forall r. (forall s. SST s r) -> r

#if __GLASGOW_HASKELL__ >= 200
# define REAL_WORLD RealWorld
# define MUT_ARRAY  MutableArray
#else
# define REAL_WORLD _RealWorld
# define MUT_ARRAY  _MutableArray
#endif

runSST :: SST REAL_WORLD r  -> r
runSST m = case m realWorld# of SST_R r s -> r

unsafeInterleaveSST :: SST s r -> SST s r
unsafeInterleaveSST m s = SST_R r s		-- Duplicates the state!
			where
			  SST_R r _ = m s

returnSST :: r -> SST s r
thenSST   :: SST s r -> (r -> State# s -> b) -> State# s -> b
thenSST_  :: SST s r -> (State# s -> b) -> State# s -> b
fixSST    :: (r -> SST s r) -> SST s r
{-# INLINE returnSST #-}
{-# INLINE thenSST #-}
{-# INLINE thenSST_ #-}

-- Hence:
--	thenSST :: SST s r -> (r -> SST  s r')     -> SST  s r'
-- and  thenSST :: SST s r -> (r -> FSST s r' err) -> FSST s r' err

-- Hence:
--	thenSST_ :: SST s r -> SST  s r'     -> SST  s r'
-- and  thenSST_ :: SST s r -> FSST s r' err -> FSST s r' err

thenSST  m k s = case m s of { SST_R r s' -> k r s' }

thenSST_ m k s = case m s of { SST_R r s' -> k s' }

returnSST r s = SST_R r s

fixSST m s = result
	   where
	     result 	  = m loop s
	     SST_R loop _ = result
\end{code}


\section{FSST: the failable strict state transformer monad}
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
data FSST_R s r err
  = FSST_R_OK   r   (State# s)
  | FSST_R_Fail err (State# s)

type FSST s r err = State# s -> FSST_R s r err
\end{code}

\begin{code}
failFSST    :: err -> FSST s r err
fixFSST     :: (r -> FSST s r err) -> FSST s r err
recoverFSST :: (err -> FSST s r err) -> FSST s r err -> FSST s r err
recoverSST  :: (err -> SST s r) -> FSST s r err -> SST s r
returnFSST  :: r -> FSST s r err
thenFSST    :: FSST s r err -> (r -> FSST s r' err) -> FSST s r' err
thenFSST_   :: FSST s r err -> FSST s r' err -> FSST s r' err
{-# INLINE failFSST #-}
{-# INLINE returnFSST #-}
{-# INLINE thenFSST #-}
{-# INLINE thenFSST_ #-}

thenFSST m k s = case m s of
		   FSST_R_OK r s'     -> k r s'
		   FSST_R_Fail err s' -> FSST_R_Fail err s'

thenFSST_ m k s = case m s of
		    FSST_R_OK r s'     -> k s'
		    FSST_R_Fail err s' -> FSST_R_Fail err s'

returnFSST r s = FSST_R_OK r s

failFSST err s = FSST_R_Fail err s

recoverFSST recovery_fn m s
  = case m s of 
	FSST_R_OK r s'     -> FSST_R_OK r s'
	FSST_R_Fail err s' -> recovery_fn err s'

recoverSST recovery_fn m s
  = case m s of 
	FSST_R_OK r s'     -> SST_R r s'
	FSST_R_Fail err s' -> recovery_fn err s'

fixFSST m s = result
	    where
	      result 	       = m loop s
	      FSST_R_OK loop _ = result
\end{code}

Mutables
~~~~~~~~
Here we implement mutable variables.  ToDo: get rid of the array impl.

\begin{code}
newMutVarSST   :: a -> SST s (MutableVar s a)
readMutVarSST  :: MutableVar s a -> SST s a
writeMutVarSST :: MutableVar s a -> a -> SST s ()

newMutVarSST init s#
  = case (newArray# 1# init s#)     of { StateAndMutableArray# s2# arr# ->
    SST_R (MUT_ARRAY vAR_IXS arr#) s2# }
  where
    vAR_IXS = error "Shouldn't access `bounds' of a MutableVar\n"

readMutVarSST (MUT_ARRAY _ var#) s#
  = case readArray# var# 0# s#	of { StateAndPtr# s2# r ->
    SST_R r s2# }

writeMutVarSST (MUT_ARRAY _ var#) val s#
  = case writeArray# var# 0# val s# of { s2# ->
    SST_R () s2# }
\end{code}

