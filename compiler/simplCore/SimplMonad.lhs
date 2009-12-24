%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplMonad]{The simplifier Monad}

\begin{code}
module SimplMonad (
	-- The monad
	SimplM,
	initSmpl,
	getDOptsSmpl, getSimplRules, getFamEnvs,

        -- Unique supply
        MonadUnique(..), newId,

	-- Counting
	SimplCount, tick, freeTick,
	getSimplCount, zeroSimplCount, pprSimplCount, 
	plusSimplCount, isZeroSimplCount,

	-- Switch checker
	SwitchChecker, SwitchResult(..), getSimplIntSwitch,
	isAmongSimpl, intSwitchSet, switchIsOn, allOffSwitchChecker
    ) where

import Id		( Id, mkSysLocal )
import Type             ( Type )
import FamInstEnv	( FamInstEnv )
import Rules		( RuleBase )
import UniqSupply
import DynFlags		( DynFlags )
import Maybes		( expectJust )
import CoreMonad
import FastString
import Outputable
import FastTypes

import Data.Array
import Data.Array.Base (unsafeAt)
\end{code}

%************************************************************************
%*									*
\subsection{Monad plumbing}
%*									*
%************************************************************************

For the simplifier monad, we want to {\em thread} a unique supply and a counter.
(Command-line switches move around through the explicitly-passed SimplEnv.)

\begin{code}
newtype SimplM result
  =  SM  { unSM :: SimplTopEnv	-- Envt that does not change much
		-> UniqSupply	-- We thread the unique supply because
				-- constantly splitting it is rather expensive
		-> SimplCount 
		-> (result, UniqSupply, SimplCount)}

data SimplTopEnv = STE	{ st_flags :: DynFlags 
			, st_rules :: RuleBase
			, st_fams  :: (FamInstEnv, FamInstEnv) }
\end{code}

\begin{code}
initSmpl :: DynFlags -> RuleBase -> (FamInstEnv, FamInstEnv) 
	 -> UniqSupply		-- No init count; set to 0
	 -> SimplM a
	 -> (a, SimplCount)

initSmpl dflags rules fam_envs us m
  = case unSM m env us (zeroSimplCount dflags) of 
	(result, _, count) -> (result, count)
  where
    env = STE { st_flags = dflags, st_rules = rules, st_fams = fam_envs }

{-# INLINE thenSmpl #-}
{-# INLINE thenSmpl_ #-}
{-# INLINE returnSmpl #-}

instance Monad SimplM where
   (>>)   = thenSmpl_
   (>>=)  = thenSmpl
   return = returnSmpl

returnSmpl :: a -> SimplM a
returnSmpl e = SM (\_st_env us sc -> (e, us, sc))

thenSmpl  :: SimplM a -> (a -> SimplM b) -> SimplM b
thenSmpl_ :: SimplM a -> SimplM b -> SimplM b

thenSmpl m k 
  = SM (\ st_env us0 sc0 ->
	  case (unSM m st_env us0 sc0) of 
		(m_result, us1, sc1) -> unSM (k m_result) st_env us1 sc1 )

thenSmpl_ m k 
  = SM (\st_env us0 sc0 ->
	 case (unSM m st_env us0 sc0) of 
		(_, us1, sc1) -> unSM k st_env us1 sc1)

-- TODO: this specializing is not allowed
-- {-# SPECIALIZE mapM         :: (a -> SimplM b) -> [a] -> SimplM [b] #-}
-- {-# SPECIALIZE mapAndUnzipM :: (a -> SimplM (b, c)) -> [a] -> SimplM ([b],[c]) #-}
-- {-# SPECIALIZE mapAccumLM   :: (acc -> b -> SimplM (acc,c)) -> acc -> [b] -> SimplM (acc, [c]) #-}
\end{code}


%************************************************************************
%*									*
\subsection{The unique supply}
%*									*
%************************************************************************

\begin{code}
instance MonadUnique SimplM where
    getUniqueSupplyM
       = SM (\_st_env us sc -> case splitUniqSupply us of
                                (us1, us2) -> (us1, us2, sc))

    getUniqueM
       = SM (\_st_env us sc -> case splitUniqSupply us of
                                (us1, us2) -> (uniqFromSupply us1, us2, sc))

    getUniquesM
        = SM (\_st_env us sc -> case splitUniqSupply us of
                                (us1, us2) -> (uniqsFromSupply us1, us2, sc))

getDOptsSmpl :: SimplM DynFlags
getDOptsSmpl = SM (\st_env us sc -> (st_flags st_env, us, sc))

getSimplRules :: SimplM RuleBase
getSimplRules = SM (\st_env us sc -> (st_rules st_env, us, sc))

getFamEnvs :: SimplM (FamInstEnv, FamInstEnv)
getFamEnvs = SM (\st_env us sc -> (st_fams st_env, us, sc))

newId :: FastString -> Type -> SimplM Id
newId fs ty = do uniq <- getUniqueM
                 return (mkSysLocal fs uniq ty)
\end{code}


%************************************************************************
%*									*
\subsection{Counting up what we've done}
%*									*
%************************************************************************

\begin{code}
getSimplCount :: SimplM SimplCount
getSimplCount = SM (\_st_env us sc -> (sc, us, sc))

tick :: Tick -> SimplM ()
tick t 
   = SM (\_st_env us sc -> let sc' = doSimplTick t sc 
                           in sc' `seq` ((), us, sc'))

freeTick :: Tick -> SimplM ()
-- Record a tick, but don't add to the total tick count, which is
-- used to decide when nothing further has happened
freeTick t 
   = SM (\_st_env us sc -> let sc' = doFreeSimplTick t sc
                           in sc' `seq` ((), us, sc'))
\end{code}


%************************************************************************
%*									*
\subsubsection{Command-line switches}
%*									*
%************************************************************************

\begin{code}
type SwitchChecker = SimplifierSwitch -> SwitchResult

data SwitchResult
  = SwBool	Bool		-- on/off
  | SwString	FastString	-- nothing or a String
  | SwInt	Int		-- nothing or an Int

allOffSwitchChecker :: SwitchChecker
allOffSwitchChecker _ = SwBool False

isAmongSimpl :: [SimplifierSwitch] -> SimplifierSwitch -> SwitchResult
isAmongSimpl on_switches		-- Switches mentioned later occur *earlier*
					-- in the list; defaults right at the end.
  = let
	tidied_on_switches = foldl rm_dups [] on_switches
		-- The fold*l* ensures that we keep the latest switches;
		-- ie the ones that occur earliest in the list.

	sw_tbl :: Array Int SwitchResult
	sw_tbl = (array	(0, lAST_SIMPL_SWITCH_TAG) -- bounds...
			all_undefined)
		 // defined_elems

	all_undefined = [ (i, SwBool False) | i <- [0 .. lAST_SIMPL_SWITCH_TAG ] ]

	defined_elems = map mk_assoc_elem tidied_on_switches
    in
    -- (avoid some unboxing, bounds checking, and other horrible things:)
    \ switch -> unsafeAt sw_tbl $ iBox (tagOf_SimplSwitch switch)
  where
    mk_assoc_elem k
	= (iBox (tagOf_SimplSwitch k), SwBool True) -- I'm here, Mom!

    -- cannot have duplicates if we are going to use the array thing
    rm_dups switches_so_far switch
      = if switch `is_elem` switches_so_far
    	then switches_so_far
	else switch : switches_so_far
      where
	_  `is_elem` []     = False
	sw `is_elem` (s:ss) = (tagOf_SimplSwitch sw) ==# (tagOf_SimplSwitch s)
			    || sw `is_elem` ss
\end{code}

\begin{code}
getSimplIntSwitch :: SwitchChecker -> (Int-> SimplifierSwitch) -> Int
getSimplIntSwitch chkr switch
  = expectJust "getSimplIntSwitch" (intSwitchSet chkr switch)

switchIsOn :: (switch -> SwitchResult) -> switch -> Bool

switchIsOn lookup_fn switch
  = case (lookup_fn switch) of
      SwBool False -> False
      _	    	   -> True

intSwitchSet :: (switch -> SwitchResult)
	     -> (Int -> switch)
	     -> Maybe Int

intSwitchSet lookup_fn switch
  = case (lookup_fn (switch (panic "intSwitchSet"))) of
      SwInt int -> Just int
      _	    	-> Nothing
\end{code}


These things behave just like enumeration types.

\begin{code}
instance Eq SimplifierSwitch where
    a == b = tagOf_SimplSwitch a ==# tagOf_SimplSwitch b

instance Ord SimplifierSwitch where
    a <  b  = tagOf_SimplSwitch a <# tagOf_SimplSwitch b
    a <= b  = tagOf_SimplSwitch a <=# tagOf_SimplSwitch b


tagOf_SimplSwitch :: SimplifierSwitch -> FastInt
tagOf_SimplSwitch NoCaseOfCase			= _ILIT(1)

-- If you add anything here, be sure to change lAST_SIMPL_SWITCH_TAG, too!

lAST_SIMPL_SWITCH_TAG :: Int
lAST_SIMPL_SWITCH_TAG = 2
\end{code}

