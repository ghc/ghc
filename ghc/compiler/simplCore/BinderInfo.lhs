%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
%************************************************************************
%*									*
\section[BinderInfo]{Information attached to binders by SubstAnal}
%*									*
%************************************************************************

\begin{code}
#include "HsVersions.h"

module BinderInfo (
	BinderInfo(..),
	FunOrArg, DuplicationDanger, InsideSCC,  -- NB: all abstract (yay!)

	addBinderInfo, orBinderInfo, andBinderInfo,

	deadOccurrence, argOccurrence, funOccurrence, dangerousArgOcc, noBinderInfo,

	markMany, markDangerousToDup, markInsideSCC,
	getBinderInfoArity,
	setBinderInfoArityToZero,

	okToInline, isOneOcc, isOneFunOcc, isOneSafeFunOcc, isDeadOcc,

	isFun, isDupDanger -- for Simon Marlow deforestation
    ) where

IMP_Ubiq(){-uitous-}

import Pretty
import Util		( panic )
#if __GLASGOW_HASKELL__ >= 202
import Outputable 
#endif

\end{code}

The @BinderInfo@ describes how a variable is used in a given scope.

NOTE: With SCCs we have to be careful what we unfold! We don't want to
change the attribution of execution costs. If we decide to unfold
within an SCC we can tag the definition as @DontKeepBinder@.
Definitions tagged as @KeepBinder@ are discarded when we enter the
scope of an SCC.

\begin{code}
data BinderInfo
  = DeadCode	-- Dead code; discard the binding.

  | ManyOcc	-- Everything else besides DeadCode and OneOccs

	Int	-- number of arguments on stack when called; this is a minimum guarantee


  | OneOcc	-- Just one occurrence (or one each in
		-- mutually-exclusive case alts).

      FunOrArg	-- How it occurs

      DuplicationDanger

      InsideSCC

      Int	-- Number of mutually-exclusive case alternatives
		-- in which it occurs

		-- Note that we only worry about the case-alt counts
		-- if the OneOcc is substitutable -- that's the only
		-- time we *use* the info; we could be more clever for
		-- other cases if we really had to. (WDP/PS)

      Int	-- number of arguments on stack when called; minimum guarantee

-- In general, we are feel free to substitute unless
-- (a) is in an argument position (ArgOcc)
-- (b) is inside a lambda [or type lambda?] (DupDanger)
-- (c) is inside an SCC expression (InsideSCC)
-- (d) is in the RHS of a binding for a variable with an INLINE pragma
--	(because the RHS will be inlined regardless of its size)
--	[again, DupDanger]

data FunOrArg
  = FunOcc 	-- An occurrence in a function position
  | ArgOcc	-- Other arg occurrence

    -- When combining branches of a case, only report FunOcc if
    -- both branches are FunOccs

data DuplicationDanger
  = DupDanger	-- Inside a non-linear lambda (that is, a lambda which
		-- is sure to be instantiated only once), or inside
		-- the rhs of an INLINE-pragma'd thing.  Either way,
		-- substituting a redex for this occurrence is
		-- dangerous because it might duplicate work.

  | NoDupDanger	-- It's ok; substitution won't duplicate work.

data InsideSCC
  = InsideSCC	    -- Inside an SCC; so be careful when substituting.
  | NotInsideSCC    -- It's ok.

noBinderInfo = ManyOcc 0	-- A non-committal value
\end{code}



\begin{code}
isOneOcc :: BinderInfo -> Bool
isOneOcc (OneOcc _ _ _ _ _) = True
isOneOcc other_bind 	    = False

isOneFunOcc :: BinderInfo -> Bool
isOneFunOcc (OneOcc FunOcc _ _ _ _) = True
isOneFunOcc other_bind 	    	    = False

isOneSafeFunOcc :: Bool -> BinderInfo -> Bool
isOneSafeFunOcc ok_to_dup (OneOcc FunOcc NoDupDanger NotInsideSCC n_alts _)
  = ok_to_dup || n_alts <= 1
isOneSafeFunOcc ok_to_dup other_bind	    = False

isDeadOcc :: BinderInfo -> Bool
isDeadOcc DeadCode = True
isDeadOcc other    = False

isFun :: FunOrArg -> Bool
isFun FunOcc = True
isFun _ = False

isDupDanger :: DuplicationDanger -> Bool
isDupDanger DupDanger = True
isDupDanger _ = False
\end{code}


\begin{code}
okToInline :: Bool		-- The thing is WHNF or bottom; 
	   -> Bool		-- It's small enough to duplicate the code
	   -> BinderInfo
	   -> Bool		-- True <=> inline it

-- A non-WHNF can be inlined if it doesn't occur inside a lambda,
-- and occurs exactly once or 
--     occurs once in each branch of a case and is small
okToInline False small_enough (OneOcc _ NoDupDanger _ n_alts _)
  = n_alts <= 1 || small_enough

-- If the thing isn't a redex, there's no danger of duplicating work, 
-- so we can inline if it occurs once, or is small
okToInline True small_enough occ_info 
 = small_enough || one_occ
 where
   one_occ = case occ_info of
		OneOcc _ _ _ n_alts _ -> n_alts <= 1
		other		      -> False

okToInline whnf_or_bot small_enough any_occ = False
\end{code}


Construction
~~~~~~~~~~~~~
\begin{code}
deadOccurrence :: BinderInfo
deadOccurrence = DeadCode

argOccurrence, funOccurrence :: Int -> BinderInfo

funOccurrence = OneOcc FunOcc NoDupDanger NotInsideSCC 1
argOccurrence = OneOcc ArgOcc NoDupDanger NotInsideSCC 1

markMany, markDangerousToDup, markInsideSCC :: BinderInfo -> BinderInfo

markMany (OneOcc _ _ _ _ ar) = ManyOcc ar
markMany (ManyOcc ar) 	     = ManyOcc ar
markMany DeadCode	     = panic "markMany"

markDangerousToDup (OneOcc posn _ in_scc n_alts ar)
  = OneOcc posn DupDanger in_scc n_alts ar
markDangerousToDup other = other

dangerousArgOcc = OneOcc ArgOcc DupDanger NotInsideSCC 1 0

markInsideSCC (OneOcc posn dup_danger _ n_alts ar)
  = OneOcc posn dup_danger InsideSCC n_alts ar
markInsideSCC other = other

addBinderInfo, orBinderInfo
	:: BinderInfo -> BinderInfo -> BinderInfo

addBinderInfo DeadCode info2 = info2
addBinderInfo info1 DeadCode = info1
addBinderInfo info1 info2
 = case (min (getBinderInfoArity info1) (getBinderInfoArity info2)) of
     (I# i#) -> ManyOcc (I# i#)
      -- ManyOcc min (getBinderInfoArity info1) (getBinderInfoArity info2))

-- (orBinderInfo orig new) is used when combining occurrence 
-- info from branches of a case

orBinderInfo DeadCode info2 = info2
orBinderInfo info1 DeadCode = info1
orBinderInfo (OneOcc posn1 dup1 scc1 n_alts1 ar_1)
	     (OneOcc posn2 dup2 scc2 n_alts2 ar_2)
  = let
      -- Seriously maligned in order to make it stricter,
      -- let's hope it is worth it..
     posn = combine_posns posn1 posn2
     scc  = combine_sccs  scc1  scc2
     dup  = combine_dups  dup1  dup2
     alts = n_alts1 + n_alts2
     ar   = min ar_1 ar_2

      -- No CSE, please!
     cont1 = case scc  of { InsideSCC -> cont2; _ -> cont2 }
     cont2 = case dup  of { DupDanger -> cont3; _ -> cont3 }
     cont3 = case alts of { (I# 0#)   -> cont4; _ -> cont4 }
     cont4 = case ar   of { (I# 0#)   -> cont5; _ -> cont5 }
     cont5 = OneOcc posn dup scc alts ar
    in
    case posn of { FunOcc -> cont1; _ -> cont1 }
orBinderInfo info1 info2
 = case (min (getBinderInfoArity info1) (getBinderInfoArity info2)) of
     (I# i#) -> ManyOcc (I# i#)

-- (andBinderInfo orig new) is used in two situations:
-- First, when a variable whose occurrence info
--   is currently "orig" is bound to a variable whose occurrence info is "new"
--	eg  (\new -> e) orig
--   What we want to do is to *worsen* orig's info to take account of new's
--
-- second, when completing a let-binding
--	let new = ...orig...
-- we compute the way orig occurs in (...orig...), and then use orBinderInfo
-- to worsen this info by the way new occurs in the let body; then we use
-- that to worsen orig's currently recorded occurrence info.

andBinderInfo DeadCode info2 = DeadCode
andBinderInfo info1 DeadCode = DeadCode
andBinderInfo (OneOcc posn1 dup1 scc1 (I# n_alts1#) (I# ar_1#))
	      (OneOcc posn2 dup2 scc2 (I# n_alts2#) ar_2)
  = let
      -- Perversly maligned in order to make it stricter.
     posn = combine_posns posn1 posn2
     scc  = combine_sccs  scc1  scc2
     dup  = combine_dups  dup1  dup2
     alts = I# (n_alts1# +# n_alts2#)

      -- No CSE, please!
     cont1 = case scc  of { InsideSCC -> cont2; _ -> cont2 }
     cont2 = case dup  of { DupDanger -> cont3; _ -> cont3 }
     cont3 = case alts of { (I# 0#) -> cont4;   _ -> cont4 }
     cont4 = OneOcc posn dup scc alts (I# ar_1#)
    in
    case posn of {FunOcc -> cont1; _ -> cont1}

andBinderInfo info1 info2 = 
 case getBinderInfoArity info1 of
   (I# i#) -> ManyOcc (I# i#)
               --ManyOcc (getBinderInfoArity info1)


combine_posns FunOcc FunOcc = FunOcc -- see comment at FunOrArg defn
combine_posns _  	 _  = ArgOcc

combine_dups DupDanger _ = DupDanger	-- Too paranoid?? ToDo
combine_dups _ DupDanger = DupDanger
combine_dups _ _	     = NoDupDanger

combine_sccs InsideSCC _ = InsideSCC	-- Too paranoid?? ToDo
combine_sccs _ InsideSCC = InsideSCC
combine_sccs _ _	     = NotInsideSCC

setBinderInfoArityToZero :: BinderInfo -> BinderInfo
setBinderInfoArityToZero DeadCode    = DeadCode
setBinderInfoArityToZero (ManyOcc _) = ManyOcc 0
setBinderInfoArityToZero (OneOcc fa dd sc i _) = OneOcc fa dd sc i 0
\end{code}

\begin{code}
getBinderInfoArity (DeadCode) = 0
getBinderInfoArity (ManyOcc i) = i
getBinderInfoArity (OneOcc _ _ _ _ i) = i
\end{code}

\begin{code}
instance Outputable BinderInfo where
  ppr sty DeadCode     = ptext SLIT("Dead")
  ppr sty (ManyOcc ar) = hcat [ ptext SLIT("Many-"), int ar ]
  ppr sty (OneOcc posn dup_danger in_scc n_alts ar)
    = hcat [ ptext SLIT("One-"), pp_posn posn, char '-', pp_danger dup_danger,
		  char '-', pp_scc in_scc,  char '-', int n_alts,
		  char '-', int ar ]
    where
      pp_posn FunOcc = ptext SLIT("fun")
      pp_posn ArgOcc = ptext SLIT("arg")

      pp_danger DupDanger   = ptext SLIT("*dup*")
      pp_danger NoDupDanger = ptext SLIT("nodup")

      pp_scc InsideSCC	  = ptext SLIT("*SCC*")
      pp_scc NotInsideSCC = ptext SLIT("noscc")
\end{code}

