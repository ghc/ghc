%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
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

	inlineUnconditionally, oneTextualOcc, oneSafeOcc,

	combineBinderInfo, combineAltsBinderInfo,

	argOccurrence, funOccurrence,
	markMany, markDangerousToDup, markInsideSCC,
	getBinderInfoArity,
	setBinderInfoArityToZero,
	
	isFun, isDupDanger -- for Simon Marlow deforestation
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)

import PlainCore
import Outputable
import Pretty
import Util		-- for pragmas only
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

	Int	-- number of arguments on stack when called


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

      Int	-- number of arguments on stack when called

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
\end{code}


Predicates
~~~~~~~~~~

@oneTextualOcc@ checks for one occurrence, in any position.
The occurrence may be inside a lambda, that's all right.

\begin{code}
oneTextualOcc :: Bool -> BinderInfo -> Bool

oneTextualOcc ok_to_dup (OneOcc _ _ _ n_alts _) = n_alts <= 1 || ok_to_dup
oneTextualOcc _         other		        = False
\end{code}

@safeSingleOcc@ detects single occurences of values that are safe to 
inline, {\em including} ones in an argument position.

\begin{code}
oneSafeOcc :: Bool -> BinderInfo -> Bool
oneSafeOcc ok_to_dup (OneOcc _ NoDupDanger NotInsideSCC n_alts _) 
						     = n_alts <= 1 || ok_to_dup
oneSafeOcc _         other			     = False
\end{code}

@inlineUnconditionally@ decides whether a let-bound thing can
definitely be inlined.

\begin{code}
inlineUnconditionally :: Bool -> BinderInfo -> Bool

--inlineUnconditionally ok_to_dup DeadCode = True
inlineUnconditionally ok_to_dup (OneOcc FunOcc NoDupDanger NotInsideSCC n_alt_occs _)
  = n_alt_occs <= 1 || ok_to_dup
	    -- We [i.e., Patrick] don't mind the code explosion,
	    -- though.  We could have a flag to limit the
	    -- damage, e.g., limit to M alternatives.

inlineUnconditionally _ _ = False
\end{code}

\begin{code}
isFun :: FunOrArg -> Bool
isFun FunOcc = True
isFun _ = False

isDupDanger :: DuplicationDanger -> Bool
isDupDanger DupDanger = True
isDupDanger _ = False
\end{code}


Construction
~~~~~~~~~~~~~
\begin{code}
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

markInsideSCC (OneOcc posn dup_danger _ n_alts ar)
  = OneOcc posn dup_danger InsideSCC n_alts ar
markInsideSCC other = other

combineBinderInfo, combineAltsBinderInfo 
	:: BinderInfo -> BinderInfo -> BinderInfo

combineBinderInfo DeadCode info2 = info2
combineBinderInfo info1 DeadCode = info1
combineBinderInfo info1 info2	 
	= ManyOcc (min (getBinderInfoArity info1) (getBinderInfoArity info2))

combineAltsBinderInfo DeadCode info2 = info2
combineAltsBinderInfo info1 DeadCode = info1
combineAltsBinderInfo (OneOcc posn1 dup1 scc1 n_alts1 ar_1)
		      (OneOcc posn2 dup2 scc2 n_alts2 ar_2)
  = OneOcc (combine_posns posn1 posn2)
	   (combine_dups  dup1  dup2)
	   (combine_sccs  scc1  scc2)
	   (n_alts1 + n_alts2)
	   (min ar_1 ar_2)
  where
    combine_posns FunOcc FunOcc = FunOcc -- see comment at FunOrArg defn
    combine_posns _  	 _      = ArgOcc

    combine_dups DupDanger _ = DupDanger	-- Too paranoid?? ToDo
    combine_dups _ DupDanger = DupDanger
    combine_dups _ _	     = NoDupDanger

    combine_sccs InsideSCC _ = InsideSCC	-- Too paranoid?? ToDo
    combine_sccs _ InsideSCC = InsideSCC
    combine_sccs _ _	     = NotInsideSCC

combineAltsBinderInfo info1 info2
	= ManyOcc (min (getBinderInfoArity info1) (getBinderInfoArity info2))

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
  ppr sty DeadCode     = ppStr "Dead"
  ppr sty (ManyOcc ar) = ppBesides [ ppStr "Many-", ppInt ar ]
  ppr sty (OneOcc posn dup_danger in_scc n_alts ar)
    = ppBesides [ ppStr "One-", pp_posn posn, ppChar '-', pp_danger dup_danger,
		  ppChar '-', pp_scc in_scc,  ppChar '-', ppInt n_alts,
		  ppChar '-', ppInt ar ]
    where
      pp_posn FunOcc = ppStr "fun"
      pp_posn ArgOcc = ppStr "arg"

      pp_danger DupDanger   = ppStr "*dup*"
      pp_danger NoDupDanger = ppStr "nodup"

      pp_scc InsideSCC	  = ppStr "*SCC*"
      pp_scc NotInsideSCC = ppStr "noscc"
\end{code}

