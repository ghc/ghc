%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
%************************************************************************
%*									*
\section[BinderInfo]{Information attached to binders by SubstAnal}
%*									*
%************************************************************************

\begin{code}
module BinderInfo (
	BinderInfo(..),

	addBinderInfo, orBinderInfo,

	deadOccurrence, funOccurrence, noBinderInfo,

	markMany, markInsideLam, markInsideSCC,
	getBinderInfoArity,
	setBinderInfoArityToZero,

	occInfoToInlinePrag
    ) where

#include "HsVersions.h"

import IdInfo		( InlinePragInfo(..), OccInfo(..) )
import GlaExts		( Int(..), (+#) )
import Outputable
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

	!Int	-- number of arguments on stack when called; this is a minimum guarantee


  | OneOcc	-- Just one occurrence (or one each in
		-- mutually-exclusive case alts).

      !OccInfo

      !InsideSCC

      !Int	-- Number of mutually-exclusive case alternatives
		-- in which it occurs

		-- Note that we only worry about the case-alt counts
		-- if the OneOcc is substitutable -- that's the only
		-- time we *use* the info; we could be more clever for
		-- other cases if we really had to. (WDP/PS)

      !Int	-- number of arguments on stack when called; minimum guarantee

-- In general, we are feel free to substitute unless
-- (a) is in an argument position (ArgOcc)
-- (b) is inside a lambda [or type lambda?] (DupDanger)
-- (c) is inside an SCC expression (InsideSCC)
-- (d) is in the RHS of a binding for a variable with an INLINE pragma
--	(because the RHS will be inlined regardless of its size)
--	[again, DupDanger]

data InsideSCC
  = InsideSCC	    -- Inside an SCC; so be careful when substituting.
  | NotInsideSCC    -- It's ok.

noBinderInfo = ManyOcc 0	-- A non-committal value
\end{code} 

\begin{code}
occInfoToInlinePrag :: BinderInfo -> InlinePragInfo
occInfoToInlinePrag DeadCode				    = IAmDead
occInfoToInlinePrag (OneOcc occ_info NotInsideSCC n_alts _) = ICanSafelyBeINLINEd occ_info (n_alts==1)
occInfoToInlinePrag other 				    = NoInlinePragInfo
\end{code}



Construction
~~~~~~~~~~~~~
\begin{code}
deadOccurrence :: BinderInfo
deadOccurrence = DeadCode

funOccurrence :: Int -> BinderInfo
funOccurrence = OneOcc NotInsideLam NotInsideSCC 1

markMany, markInsideLam, markInsideSCC :: BinderInfo -> BinderInfo

markMany (OneOcc _ _ _ ar) = ManyOcc ar
markMany (ManyOcc ar) 	   = ManyOcc ar
markMany DeadCode	   = panic "markMany"

markInsideLam (OneOcc _ in_scc n_alts ar) = OneOcc InsideLam in_scc n_alts ar
markInsideLam other		 	  = other

markInsideSCC (OneOcc dup_danger _ n_alts ar) = OneOcc dup_danger InsideSCC n_alts ar
markInsideSCC other			      = other

addBinderInfo, orBinderInfo :: BinderInfo -> BinderInfo -> BinderInfo

addBinderInfo DeadCode info2 = info2
addBinderInfo info1 DeadCode = info1
addBinderInfo info1 info2
 = ManyOcc (min (getBinderInfoArity info1) (getBinderInfoArity info2))

-- (orBinderInfo orig new) is used
-- when combining occurrence info from branches of a case

orBinderInfo DeadCode info2 = info2
orBinderInfo info1 DeadCode = info1
orBinderInfo (OneOcc dup1 scc1 n_alts1 ar_1)
	     (OneOcc dup2 scc2 n_alts2 ar_2)
  = let
     scc  = or_sccs  scc1  scc2
     dup  = or_dups  dup1  dup2
     alts = n_alts1 + n_alts2
     ar   = min ar_1 ar_2
   in
   OneOcc dup scc alts ar

orBinderInfo info1 info2
 = ManyOcc (min (getBinderInfoArity info1) (getBinderInfoArity info2))

or_dups InsideLam _         = InsideLam
or_dups _         InsideLam = InsideLam
or_dups _         _         = NotInsideLam

or_sccs InsideSCC _ = InsideSCC
or_sccs _ InsideSCC = InsideSCC
or_sccs _ _	    = NotInsideSCC

setBinderInfoArityToZero :: BinderInfo -> BinderInfo
setBinderInfoArityToZero DeadCode    = DeadCode
setBinderInfoArityToZero (ManyOcc _) = ManyOcc 0
setBinderInfoArityToZero (OneOcc dd sc i _) = OneOcc dd sc i 0
\end{code}

\begin{code}
getBinderInfoArity (DeadCode) = 0
getBinderInfoArity (ManyOcc i) = i
getBinderInfoArity (OneOcc _ _ _ i) = i
\end{code}

\begin{code}
instance Outputable BinderInfo where
  ppr DeadCode     = ptext SLIT("Dead")
  ppr (ManyOcc ar) = hcat [ ptext SLIT("Many-"), int ar ]
  ppr (OneOcc dup_danger in_scc n_alts ar)
    = hcat [ ptext SLIT("One-"), ppr dup_danger,
		  char '-', pp_scc in_scc,  char '-', int n_alts,
		  char '-', int ar ]
    where
      pp_scc InsideSCC	  = ptext SLIT("*SCC*")
      pp_scc NotInsideSCC = ptext SLIT("noscc")
\end{code}

