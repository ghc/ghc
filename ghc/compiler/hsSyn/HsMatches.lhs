%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsMatches]{Abstract syntax: matches and guarded right-hand-sides}

The @Match@, @GRHSsAndBinds@ and @GRHS@ datatypes.

\begin{code}
#include "HsVersions.h"

module HsMatches where

IMP_Ubiq(){-uitous-}

-- Friends
import HsExpr		( HsExpr, Stmt )
import HsBinds		( HsBinds, nullBinds )

-- Others
import Outputable	( ifPprShowAll, PprStyle )
import PprType		( GenType{-instance Outputable-} )
import Pretty
import SrcLoc		( SrcLoc{-instances-} )
import Util		( panic )
import Outputable	( Outputable(..) )
#if __GLASGOW_HASKELL__ >= 202
import Name
#endif
       
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

  | SimpleMatch	    (HsExpr tyvar uvar id pat)		-- Used in translations
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
  = GRHS	    [Stmt tyvar uvar id pat]	-- guard(ed)...
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
		PprStyle -> (Bool, Doc) -> [Match tyvar uvar id pat] -> Doc

pprMatches sty print_info@(is_case, name) [match]
  = if is_case then
    	pprMatch sty is_case match
    else
    	hang name 4 (pprMatch sty is_case match)

pprMatches sty print_info (match1 : rest)
 = ($$) (pprMatches sty print_info [match1])
	   (pprMatches sty print_info rest)

---------------------------------------------
pprMatch :: (NamedThing id, Outputable id, Outputable pat,
	       Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar) =>
	PprStyle -> Bool -> Match tyvar uvar id pat -> Doc

pprMatch sty is_case first_match
 = hang (sep (map (ppr sty) row_of_pats))
	8 grhss_etc_stuff
 where
    (row_of_pats, grhss_etc_stuff) = ppr_match sty is_case first_match

    ppr_match sty is_case (PatMatch pat match)
      = (pat:pats, grhss_stuff)
      where
	(pats, grhss_stuff) = ppr_match sty is_case match

    ppr_match sty is_case (GRHSMatch grhss_n_binds)
      = ([], pprGRHSsAndBinds sty is_case grhss_n_binds)

    ppr_match sty is_case (SimpleMatch expr)
      = ([], hang (text (if is_case then "->" else "="))
		 4 (ppr sty expr))

----------------------------------------------------------

pprGRHSsAndBinds :: (NamedThing id, Outputable id, Outputable pat,
	            Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar) =>
		PprStyle -> Bool -> GRHSsAndBinds tyvar uvar id pat -> Doc

pprGRHSsAndBinds sty is_case (GRHSsAndBindsIn grhss binds)
 = ($$) (vcat (map (pprGRHS sty is_case) grhss))
	   (if (nullBinds binds)
	    then empty
	    else vcat [ text "where", nest 4 (ppr sty binds) ])

pprGRHSsAndBinds sty is_case (GRHSsAndBindsOut grhss binds ty)
 = ($$) (vcat (map (pprGRHS sty is_case) grhss))
	   (if (nullBinds binds)
	    then empty
	    else vcat [ ifPprShowAll sty
				(hsep [text "{- ty:", ppr sty ty, text "-}"]),
			    text "where", nest 4 (ppr sty binds) ])

---------------------------------------------
pprGRHS :: (NamedThing id, Outputable id, Outputable pat,
	    Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
	=> PprStyle -> Bool -> GRHS tyvar uvar id pat -> Doc

pprGRHS sty is_case (GRHS [] expr locn)
 =  hang (text (if is_case then "->" else "="))
	 4 (ppr sty expr)

pprGRHS sty is_case (GRHS guard expr locn)
 = hang (hsep [char '|', ppr sty guard, text (if is_case then "->" else "=")])
        4 (ppr sty expr)

pprGRHS sty is_case (OtherwiseGRHS  expr locn)
  = hang (text (if is_case then "->" else "="))
	 4 (ppr sty expr)
\end{code}
