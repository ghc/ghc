%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsMatches]{Abstract syntax: matches and guarded right-hand-sides}

The @Match@, @GRHSsAndBinds@ and @GRHS@ datatypes.

\begin{code}
module HsMatches where

#include "HsVersions.h"

-- Friends
import HsExpr		( HsExpr, Stmt )
import HsBinds		( HsBinds, nullBinds )

-- Others
import PprType		( GenType{-instance Outputable-} )
import SrcLoc		( SrcLoc{-instances-} )
import Util		( panic )
import Outputable
import Name		( NamedThing )
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
data Match flexi id pat
  = PatMatch	    pat
		    (Match flexi id pat)
  | GRHSMatch	    (GRHSsAndBinds flexi id pat)

  | SimpleMatch	    (HsExpr flexi id pat)		-- Used in translations
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
data GRHSsAndBinds flexi id pat
  = GRHSsAndBindsIn 	[GRHS flexi id pat]	    -- at least one GRHS
			(HsBinds flexi id pat)

  | GRHSsAndBindsOut 	[GRHS flexi id pat]	    -- at least one GRHS
			(HsBinds flexi id pat)
			(GenType flexi)

data GRHS flexi id pat
  = GRHS	    [Stmt flexi id pat]	-- guard(ed)...
		    (HsExpr flexi id pat)	-- ... right-hand side
		    SrcLoc

unguardedRHS :: (HsExpr flexi id pat) -> SrcLoc -> [GRHS flexi id pat]
unguardedRHS rhs loc = [GRHS [] rhs loc]
\end{code}

@getMatchLoc@ takes a @Match@ and returns the
source-location gotten from the GRHS inside.
THis is something of a nuisance, but no more.

\begin{code}
getMatchLoc :: Match flexi id pat -> SrcLoc
getMatchLoc (PatMatch _ m)				       = getMatchLoc m
getMatchLoc (GRHSMatch (GRHSsAndBindsIn (GRHS _ _ loc : _) _)) = loc
\end{code}

%************************************************************************
%*									*
\subsection{Printing}
%*									*
%************************************************************************

We know the list must have at least one @Match@ in it.
\begin{code}
pprMatches :: (NamedThing id, Outputable id, Outputable pat)
	   => (Bool, SDoc) -> [Match flexi id pat] -> SDoc

pprMatches print_info@(is_case, name) [match]
  = if is_case then
    	pprMatch is_case match
    else
    	name <+> (pprMatch is_case match)

pprMatches print_info (match1 : rest)
 = ($$) (pprMatches print_info [match1])
	   (pprMatches print_info rest)

---------------------------------------------
pprMatch :: (NamedThing id, Outputable id, Outputable pat)
	 => Bool -> Match flexi id pat -> SDoc

pprMatch is_case first_match
 = sep [(sep (map (ppr) row_of_pats)),
	grhss_etc_stuff]
 where
    (row_of_pats, grhss_etc_stuff) = ppr_match is_case first_match

    ppr_match is_case (PatMatch pat match)
      = (pat:pats, grhss_stuff)
      where
	(pats, grhss_stuff) = ppr_match is_case match

    ppr_match is_case (GRHSMatch grhss_n_binds)
      = ([], pprGRHSsAndBinds is_case grhss_n_binds)

    ppr_match is_case (SimpleMatch expr)
      = ([], text (if is_case then "->" else "=") <+> ppr expr)

----------------------------------------------------------

pprGRHSsAndBinds :: (NamedThing id, Outputable id, Outputable pat)
		 => Bool -> GRHSsAndBinds flexi id pat -> SDoc

pprGRHSsAndBinds is_case (GRHSsAndBindsIn grhss binds)
 = ($$) (vcat (map (pprGRHS is_case) grhss))
	   (if (nullBinds binds)
	    then empty
	    else vcat [ text "where", nest 4 (ppr binds) ])

pprGRHSsAndBinds is_case (GRHSsAndBindsOut grhss binds ty)
 = ($$) (vcat (map (pprGRHS is_case) grhss))
	   (if (nullBinds binds)
	    then empty
	    else vcat [text "where", nest 4 (ppr binds) ])

---------------------------------------------
pprGRHS :: (NamedThing id, Outputable id, Outputable pat)
	=> Bool -> GRHS flexi id pat -> SDoc

pprGRHS is_case (GRHS [] expr locn)
 =  text (if is_case then "->" else "=") <+> ppr expr

pprGRHS is_case (GRHS guard expr locn)
 = sep [char '|' <+> interpp'SP guard,
	text (if is_case then "->" else "=") <+> ppr expr
   ]
\end{code}
