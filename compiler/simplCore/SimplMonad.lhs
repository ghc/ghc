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
	SimplCount, Tick(..),
	tick, freeTick,
	getSimplCount, zeroSimplCount, pprSimplCount, 
	plusSimplCount, isZeroSimplCount,

	-- Switch checker
	SwitchChecker, SwitchResult(..), getSimplIntSwitch,
	isAmongSimpl, intSwitchSet, switchIsOn
    ) where

import Id		( Id, mkSysLocal )
import Type             ( Type )
import FamInstEnv	( FamInstEnv )
import Rules		( RuleBase )
import UniqSupply
import DynFlags		( SimplifierSwitch(..), DynFlags, DynFlag(..), dopt )
import StaticFlags	( opt_PprStyle_Debug, opt_HistorySize )
import Maybes		( expectJust )
import FiniteMap	( FiniteMap, emptyFM, lookupFM, addToFM, plusFM_C, fmToList )
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
   = SM (\_st_env us sc -> let sc' = doTick t sc 
                           in sc' `seq` ((), us, sc'))

freeTick :: Tick -> SimplM ()
-- Record a tick, but don't add to the total tick count, which is
-- used to decide when nothing further has happened
freeTick t 
   = SM (\_st_env us sc -> let sc' = doFreeTick t sc
                           in sc' `seq` ((), us, sc'))
\end{code}

\begin{code}
verboseSimplStats :: Bool
verboseSimplStats = opt_PprStyle_Debug		-- For now, anyway

zeroSimplCount	   :: DynFlags -> SimplCount
isZeroSimplCount   :: SimplCount -> Bool
pprSimplCount	   :: SimplCount -> SDoc
doTick, doFreeTick :: Tick -> SimplCount -> SimplCount
plusSimplCount     :: SimplCount -> SimplCount -> SimplCount
\end{code}

\begin{code}
data SimplCount = VerySimplZero		-- These two are used when 
		| VerySimplNonZero	-- we are only interested in 
					-- termination info

		| SimplCount	{
			ticks   :: !Int,		-- Total ticks
			details :: !TickCounts,		-- How many of each type
			n_log	:: !Int,		-- N
			log1	:: [Tick],		-- Last N events; <= opt_HistorySize
			log2	:: [Tick]		-- Last opt_HistorySize events before that
		  }

type TickCounts = FiniteMap Tick Int

zeroSimplCount dflags
		-- This is where we decide whether to do
		-- the VerySimpl version or the full-stats version
  | dopt Opt_D_dump_simpl_stats dflags
  = SimplCount {ticks = 0, details = emptyFM,
                n_log = 0, log1 = [], log2 = []}
  | otherwise
  = VerySimplZero

isZeroSimplCount VerySimplZero    	    = True
isZeroSimplCount (SimplCount { ticks = 0 }) = True
isZeroSimplCount _    			    = False

doFreeTick tick sc@SimplCount { details = dts } 
  = sc { details = dts `addTick` tick }
doFreeTick _ sc = sc 

doTick tick sc@SimplCount { ticks = tks, details = dts, n_log = nl, log1 = l1 }
  | nl >= opt_HistorySize = sc1 { n_log = 1, log1 = [tick], log2 = l1 }
  | otherwise		  = sc1 { n_log = nl+1, log1 = tick : l1 }
  where
    sc1 = sc { ticks = tks+1, details = dts `addTick` tick }

doTick _ _ = VerySimplNonZero -- The very simple case


-- Don't use plusFM_C because that's lazy, and we want to 
-- be pretty strict here!
addTick :: TickCounts -> Tick -> TickCounts
addTick fm tick = case lookupFM fm tick of
			Nothing -> addToFM fm tick 1
			Just n  -> n1 `seq` addToFM fm tick n1
				where
				   n1 = n+1


plusSimplCount sc1@(SimplCount { ticks = tks1, details = dts1 })
	       sc2@(SimplCount { ticks = tks2, details = dts2 })
  = log_base { ticks = tks1 + tks2, details = plusFM_C (+) dts1 dts2 }
  where
	-- A hackish way of getting recent log info
    log_base | null (log1 sc2) = sc1	-- Nothing at all in sc2
	     | null (log2 sc2) = sc2 { log2 = log1 sc1 }
	     | otherwise       = sc2

plusSimplCount VerySimplZero VerySimplZero = VerySimplZero
plusSimplCount _             _             = VerySimplNonZero

pprSimplCount VerySimplZero    = ptext (sLit "Total ticks: ZERO!")
pprSimplCount VerySimplNonZero = ptext (sLit "Total ticks: NON-ZERO!")
pprSimplCount (SimplCount { ticks = tks, details = dts, log1 = l1, log2 = l2 })
  = vcat [ptext (sLit "Total ticks:    ") <+> int tks,
	  text "",
	  pprTickCounts (fmToList dts),
	  if verboseSimplStats then
		vcat [text "",
		      ptext (sLit "Log (most recent first)"),
		      nest 4 (vcat (map ppr l1) $$ vcat (map ppr l2))]
	  else empty
    ]

pprTickCounts :: [(Tick,Int)] -> SDoc
pprTickCounts [] = empty
pprTickCounts ((tick1,n1):ticks)
  = vcat [int tot_n <+> text (tickString tick1),
	  pprTCDetails real_these,
	  pprTickCounts others
    ]
  where
    tick1_tag		= tickToTag tick1
    (these, others)	= span same_tick ticks
    real_these		= (tick1,n1):these
    same_tick (tick2,_) = tickToTag tick2 == tick1_tag
    tot_n		= sum [n | (_,n) <- real_these]

pprTCDetails :: [(Tick, Int)] -> SDoc
pprTCDetails ticks@((tick,_):_)
  | verboseSimplStats || isRuleFired tick
  = nest 4 (vcat [int n <+> pprTickCts tick | (tick,n) <- ticks])
  | otherwise
  = empty
pprTCDetails [] = panic "pprTCDetails []"
\end{code}

%************************************************************************
%*									*
\subsection{Ticks}
%*									*
%************************************************************************

\begin{code}
data Tick
  = PreInlineUnconditionally	Id
  | PostInlineUnconditionally	Id

  | UnfoldingDone    		Id
  | RuleFired			FastString	-- Rule name

  | LetFloatFromLet
  | EtaExpansion		Id	-- LHS binder
  | EtaReduction		Id	-- Binder on outer lambda
  | BetaReduction		Id	-- Lambda binder


  | CaseOfCase			Id	-- Bndr on *inner* case
  | KnownBranch			Id	-- Case binder
  | CaseMerge			Id	-- Binder on outer case
  | AltMerge			Id	-- Case binder
  | CaseElim			Id	-- Case binder
  | CaseIdentity		Id	-- Case binder
  | FillInCaseDefault		Id	-- Case binder

  | BottomFound		
  | SimplifierDone		-- Ticked at each iteration of the simplifier

isRuleFired :: Tick -> Bool
isRuleFired (RuleFired _) = True
isRuleFired _             = False

instance Outputable Tick where
  ppr tick = text (tickString tick) <+> pprTickCts tick

instance Eq Tick where
  a == b = case a `cmpTick` b of
           EQ -> True
           _ -> False

instance Ord Tick where
  compare = cmpTick

tickToTag :: Tick -> Int
tickToTag (PreInlineUnconditionally _)	= 0
tickToTag (PostInlineUnconditionally _)	= 1
tickToTag (UnfoldingDone _)		= 2
tickToTag (RuleFired _)			= 3
tickToTag LetFloatFromLet		= 4
tickToTag (EtaExpansion _)		= 5
tickToTag (EtaReduction _)		= 6
tickToTag (BetaReduction _)		= 7
tickToTag (CaseOfCase _)		= 8
tickToTag (KnownBranch _)		= 9
tickToTag (CaseMerge _)			= 10
tickToTag (CaseElim _)			= 11
tickToTag (CaseIdentity _)		= 12
tickToTag (FillInCaseDefault _)		= 13
tickToTag BottomFound			= 14
tickToTag SimplifierDone		= 16
tickToTag (AltMerge _)			= 17

tickString :: Tick -> String
tickString (PreInlineUnconditionally _)	= "PreInlineUnconditionally"
tickString (PostInlineUnconditionally _)= "PostInlineUnconditionally"
tickString (UnfoldingDone _)		= "UnfoldingDone"
tickString (RuleFired _)		= "RuleFired"
tickString LetFloatFromLet		= "LetFloatFromLet"
tickString (EtaExpansion _)		= "EtaExpansion"
tickString (EtaReduction _)		= "EtaReduction"
tickString (BetaReduction _)		= "BetaReduction"
tickString (CaseOfCase _)		= "CaseOfCase"
tickString (KnownBranch _)		= "KnownBranch"
tickString (CaseMerge _)		= "CaseMerge"
tickString (AltMerge _)			= "AltMerge"
tickString (CaseElim _)			= "CaseElim"
tickString (CaseIdentity _)		= "CaseIdentity"
tickString (FillInCaseDefault _)	= "FillInCaseDefault"
tickString BottomFound			= "BottomFound"
tickString SimplifierDone		= "SimplifierDone"

pprTickCts :: Tick -> SDoc
pprTickCts (PreInlineUnconditionally v)	= ppr v
pprTickCts (PostInlineUnconditionally v)= ppr v
pprTickCts (UnfoldingDone v)		= ppr v
pprTickCts (RuleFired v)		= ppr v
pprTickCts LetFloatFromLet		= empty
pprTickCts (EtaExpansion v)		= ppr v
pprTickCts (EtaReduction v)		= ppr v
pprTickCts (BetaReduction v)		= ppr v
pprTickCts (CaseOfCase v)		= ppr v
pprTickCts (KnownBranch v)		= ppr v
pprTickCts (CaseMerge v)		= ppr v
pprTickCts (AltMerge v)			= ppr v
pprTickCts (CaseElim v)			= ppr v
pprTickCts (CaseIdentity v)		= ppr v
pprTickCts (FillInCaseDefault v)	= ppr v
pprTickCts _    			= empty

cmpTick :: Tick -> Tick -> Ordering
cmpTick a b = case (tickToTag a `compare` tickToTag b) of
		GT -> GT
		EQ | isRuleFired a || verboseSimplStats -> cmpEqTick a b
		   | otherwise				-> EQ
		LT -> LT
	-- Always distinguish RuleFired, so that the stats
	-- can report them even in non-verbose mode

cmpEqTick :: Tick -> Tick -> Ordering
cmpEqTick (PreInlineUnconditionally a)	(PreInlineUnconditionally b)	= a `compare` b
cmpEqTick (PostInlineUnconditionally a)	(PostInlineUnconditionally b)	= a `compare` b
cmpEqTick (UnfoldingDone a)		(UnfoldingDone b)		= a `compare` b
cmpEqTick (RuleFired a)			(RuleFired b)			= a `compare` b
cmpEqTick (EtaExpansion a)		(EtaExpansion b)		= a `compare` b
cmpEqTick (EtaReduction a)		(EtaReduction b)		= a `compare` b
cmpEqTick (BetaReduction a)		(BetaReduction b)		= a `compare` b
cmpEqTick (CaseOfCase a)		(CaseOfCase b)			= a `compare` b
cmpEqTick (KnownBranch a)		(KnownBranch b)			= a `compare` b
cmpEqTick (CaseMerge a)			(CaseMerge b)			= a `compare` b
cmpEqTick (AltMerge a)			(AltMerge b)			= a `compare` b
cmpEqTick (CaseElim a)			(CaseElim b)			= a `compare` b
cmpEqTick (CaseIdentity a)		(CaseIdentity b)		= a `compare` b
cmpEqTick (FillInCaseDefault a)		(FillInCaseDefault b)		= a `compare` b
cmpEqTick _     			_     				= EQ
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
    mk_assoc_elem k@(MaxSimplifierIterations lvl)
	= (iBox (tagOf_SimplSwitch k), SwInt lvl)
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
tagOf_SimplSwitch (MaxSimplifierIterations _)	= _ILIT(1)
tagOf_SimplSwitch NoCaseOfCase			= _ILIT(2)

-- If you add anything here, be sure to change lAST_SIMPL_SWITCH_TAG, too!

lAST_SIMPL_SWITCH_TAG :: Int
lAST_SIMPL_SWITCH_TAG = 2
\end{code}

