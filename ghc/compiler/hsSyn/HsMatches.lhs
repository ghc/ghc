%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsMatches]{Abstract syntax: matches and guarded right-hand-sides}

The @Match@, @GRHSsAndBinds@ and @GRHS@ datatypes.

\begin{code}
#include "HsVersions.h"

module HsMatches where

import Ubiq{-uitous-}

import HsLoop		( HsExpr, nullBinds, HsBinds )
import Outputable	( ifPprShowAll )
import PprType
import Pretty
import SrcLoc		( SrcLoc{-instances-} )
import Util		( panic )
\end{code}

%************************************************************************
%*									*
\subsection{@Match@, @GRHSsAndBinds@, and @GRHS@ datatypes}
%*									*
%************************************************************************

@Match@es are sets of pattern bindings and right hand sides for
functions, patterns or case branches. For example, if a function @g@
is defined as:
\begin{verbatim}
g (x,y) = y
g ((x:ys),y) = y+1,
\end{verbatim}
then \tr{g} has two @Match@es: @(x,y) = y@ and @((x:ys),y) = y+1@.

It is always the case that each element of an @[Match]@ list has the
same number of @PatMatch@s inside it.  This corresponds to saying that
a function defined by pattern matching must have the same number of
patterns in each equation.

\begin{code}
data Match tyvar uvar id pat
  = PatMatch	    pat
		    (Match tyvar uvar id pat)
  | GRHSMatch	    (GRHSsAndBinds tyvar uvar id pat)
\end{code}

Sets of guarded right hand sides (GRHSs). In:
\begin{verbatim}
f (x,y) | x==True = y
	| otherwise = y*2
\end{verbatim}
a guarded right hand side is either
@(x==True = y)@, or @(otherwise = y*2)@.

For each match, there may be several guarded right hand
sides, as the definition of @f@ shows.

\begin{code}
data GRHSsAndBinds tyvar uvar id pat
  = GRHSsAndBindsIn 	[GRHS tyvar uvar id pat]	    -- at least one GRHS
			(HsBinds tyvar uvar id pat)

  | GRHSsAndBindsOut 	[GRHS tyvar uvar id pat]	    -- at least one GRHS
			(HsBinds tyvar uvar id pat)
			(GenType tyvar uvar)

data GRHS tyvar uvar id pat
  = GRHS	    (HsExpr tyvar uvar id pat)	-- guard(ed)...
		    (HsExpr tyvar uvar id pat)	-- ... right-hand side
		    SrcLoc

  | OtherwiseGRHS   (HsExpr tyvar uvar id pat)	-- guard-free
		    SrcLoc
\end{code}

%************************************************************************
%*									*
\subsection{Printing}
%*									*
%************************************************************************

We know the list must have at least one @Match@ in it.
\begin{code}
pprMatches :: (NamedThing id, Outputable id, Outputable pat,
	       Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar) =>
		PprStyle -> (Bool, Pretty) -> [Match tyvar uvar id pat] -> Pretty

pprMatches sty print_info@(is_case, name) [match]
  = if is_case then
    	pprMatch sty is_case match
    else
    	ppHang name 4 (pprMatch sty is_case match)

pprMatches sty print_info (match1 : rest)
 = ppAbove (pprMatches sty print_info [match1])
	   (pprMatches sty print_info rest)

---------------------------------------------
pprMatch :: (NamedThing id, Outputable id, Outputable pat,
	       Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar) =>
	PprStyle -> Bool -> Match tyvar uvar id pat -> Pretty

pprMatch sty is_case first_match
 = ppHang (ppSep (map (ppr sty) row_of_pats))
	8 grhss_etc_stuff
 where
    (row_of_pats, grhss_etc_stuff) = ppr_match sty is_case first_match

    ppr_match sty is_case (PatMatch pat match)
     = (pat:pats, grhss_stuff)
     where
	(pats, grhss_stuff) = ppr_match sty is_case match

    ppr_match sty is_case (GRHSMatch grhss_n_binds)
     = ([], pprGRHSsAndBinds sty is_case grhss_n_binds)

----------------------------------------------------------

pprGRHSsAndBinds sty is_case (GRHSsAndBindsIn grhss binds)
 = ppAbove (ppAboves (map (pprGRHS sty is_case) grhss))
	   (if (nullBinds binds)
	    then ppNil
	    else ppAboves [ ppStr "where", ppNest 4 (ppr sty binds) ])

pprGRHSsAndBinds sty is_case (GRHSsAndBindsOut grhss binds ty)
 = ppAbove (ppAboves (map (pprGRHS sty is_case) grhss))
	   (if (nullBinds binds)
	    then ppNil
	    else ppAboves [ ifPprShowAll sty
				(ppCat [ppStr "{- ty:", ppr sty ty, ppStr "-}"]),
			    ppStr "where", ppNest 4 (ppr sty binds) ])

---------------------------------------------
pprGRHS :: (NamedThing id, Outputable id, Outputable pat,
	    Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
	=> PprStyle -> Bool -> GRHS tyvar uvar id pat -> Pretty

pprGRHS sty is_case (GRHS guard expr locn)
 = ppHang (ppCat [ppChar '|', ppr sty guard, ppStr (if is_case then "->" else "=")])
        4 (ppr sty expr)

pprGRHS sty is_case (OtherwiseGRHS  expr locn)
  = ppHang (ppStr (if is_case then "->" else "="))
	 4 (ppr sty expr)
\end{code}
