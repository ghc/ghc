\section{SST: the strict state transformer monad}
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
#include "HsVersions.h"

module SST(
	SST(..), SST_R, FSST(..), FSST_R,

	_runSST,
	thenSST, thenSST_, returnSST,
	thenFSST, thenFSST_, returnFSST, failFSST,
	recoverFSST, recoverSST, fixFSST,

	MutableVar(..), _MutableArray, 
	newMutVarSST, readMutVarSST, writeMutVarSST
  ) where

import PreludeGlaST( MutableVar(..), _MutableArray(..) )

CHK_Ubiq() -- debugging consistency check
\end{code}

\begin{code}
data SST_R s r = SST_R r (State# s)
type SST   s r = State# s -> SST_R s r
\end{code}

\begin{code}
-- Type of runSST should be builtin ...
-- runSST :: forall r. (forall s. SST s r) -> r

_runSST :: SST _RealWorld r -> r
_runSST m = case m realWorld# of SST_R r s -> r


thenSST :: SST s r -> (r -> State# s -> b) -> State# s -> b
{-# INLINE thenSST #-}
-- Hence:
--	thenSST :: SST s r -> (r -> SST  s r')     -> SST  s r'
-- and  thenSST :: SST s r -> (r -> FSST s r' err) -> FSST s r' err

thenSST m k s = case m s of { SST_R r s' -> k r s' }

thenSST_ :: SST s r -> (State# s -> b) -> State# s -> b
{-# INLINE thenSST_ #-}
-- Hence:
--	thenSST_ :: SST s r -> SST  s r'     -> SST  s r'
-- and  thenSST_ :: SST s r -> FSST s r' err -> FSST s r' err

thenSST_ m k s = case m s of { SST_R r s' -> k s' }

returnSST :: r -> SST s r
{-# INLINE returnSST #-}
returnSST r s = SST_R r s
\end{code}


\section{FSST: the failable strict state transformer monad}
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
data FSST_R s r err = FSST_R_OK   r   (State# s)
		    | FSST_R_Fail err (State# s)

type FSST   s r err = State# s -> FSST_R s r err
\end{code}

\begin{code}
thenFSST :: FSST s r err -> (r -> FSST s r' err) -> FSST s r' err
{-# INLINE thenFSST #-}
thenFSST m k s = case m s of
		   FSST_R_OK r s'     -> k r s'
		   FSST_R_Fail err s' -> FSST_R_Fail err s'

thenFSST_ :: FSST s r err -> FSST s r' err -> FSST s r' err
{-# INLINE thenFSST_ #-}
thenFSST_ m k s = case m s of
		    FSST_R_OK r s'     -> k s'
		    FSST_R_Fail err s' -> FSST_R_Fail err s'

returnFSST :: r -> FSST s r err
{-# INLINE returnFSST #-}
returnFSST r s = FSST_R_OK r s

failFSST    :: err -> FSST s r err
{-# INLINE failFSST #-}
failFSST err s = FSST_R_Fail err s

recoverFSST :: (err -> FSST s r err)
	    -> FSST s r err
	    -> FSST s r err
recoverFSST recovery_fn m s
  = case m s of 
	FSST_R_OK r s'     -> FSST_R_OK r s'
	FSST_R_Fail err s' -> recovery_fn err s'

recoverSST :: (err -> SST s r)
	    -> FSST s r err
	    -> SST s r
recoverSST recovery_fn m s
  = case m s of 
	FSST_R_OK r s'     -> SST_R r s'
	FSST_R_Fail err s' -> recovery_fn err s'

fixFSST :: (r -> FSST s r err) -> FSST s r err
fixFSST m s = result
	    where
	      result 	       = m loop s
	      FSST_R_OK loop _ = result
\end{code}

Mutables
~~~~~~~~
Here we implement mutable variables.  ToDo: get rid of the array impl.

\begin{code}
newMutVarSST :: a -> SST s (MutableVar s a)
newMutVarSST init s#
  = case (newArray# 1# init s#)     of { StateAndMutableArray# s2# arr# ->
    SST_R (_MutableArray vAR_IXS arr#) s2# }
  where
    vAR_IXS = error "Shouldn't access `bounds' of a MutableVar\n"

readMutVarSST :: MutableVar s a -> SST s a
readMutVarSST (_MutableArray _ var#) s#
  = case readArray# var# 0# s#	of { StateAndPtr# s2# r ->
    SST_R r s2# }

writeMutVarSST :: MutableVar s a -> a -> SST s ()
writeMutVarSST (_MutableArray _ var#) val s#
  = case writeArray# var# 0# val s# of { s2# ->
    SST_R () s2# }
\end{code}

