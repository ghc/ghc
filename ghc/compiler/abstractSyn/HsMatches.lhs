%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[HsMatches]{Abstract syntax: matches and guarded right-hand-sides}

The @Match@, @GRHSsAndBinds@ and @GRHS@ datatypes.

\begin{code}
#include "HsVersions.h"

module HsMatches where

import AbsUniType	( UniType
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import HsBinds		( Binds, nullBinds )
import HsExpr		( Expr )
import HsPat		( ProtoNamePat(..), RenamedPat(..),
			  TypecheckedPat, InPat
			  IF_ATTACK_PRAGMAS(COMMA typeOfPat)
			)
import Name             ( Name )
import Unique           ( Unique )
import Id		( Id )
import Outputable
import Pretty
import ProtoName	( ProtoName(..) ) -- .. for pragmas only
import SrcLoc		( SrcLoc )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[AbsSyntax-Match]{@Match@}
%*									*
%************************************************************************

Sets of pattern bindings and right hand sides for
functions, patterns or case branches. For example,
if a function @g@ is defined as:
\begin{verbatim}
g (x,y) = y
g ((x:ys),y) = y+1,
\end{verbatim}
then a single @Match@ would be either @(x,y) = y@ or
@((x:ys),y) = y+1@, and @[Match]@ would be
@[((x,y) = y), (((x:ys),y) = y+1)]@.

It is always the case that each element of an @[Match]@ list has the
same number of @PatMatch@s inside it.  This corresponds to saying that
a function defined by pattern matching must have the same number of
patterns in each equation.

So, a single ``match'':
\begin{code}
data Match bdee pat
  = PatMatch	    pat
		    (Match bdee pat)
  | GRHSMatch	    (GRHSsAndBinds bdee pat)

type ProtoNameMatch	= Match ProtoName ProtoNamePat
type RenamedMatch 	= Match Name	  RenamedPat
type TypecheckedMatch	= Match Id        TypecheckedPat
\end{code}

Printing, of one and several @Matches@.
\begin{code}
pprMatch :: (NamedThing bdee, Outputable bdee, 
             NamedThing pat, Outputable pat) =>
	PprStyle -> Bool -> Match bdee pat -> Pretty

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
\end{code}

We know the list must have at least one @Match@ in it.
\begin{code}
pprMatches :: (NamedThing bdee, Outputable bdee, 
               NamedThing pat, Outputable pat) =>
		PprStyle -> (Bool, Pretty) -> [Match bdee pat] -> Pretty

pprMatches sty print_info@(is_case, name) [match]
  = if is_case then
    	pprMatch sty is_case match
    else
    	ppHang name 4 (pprMatch sty is_case match)

pprMatches sty print_info (match1 : rest)
 = ppAbove (pprMatches sty print_info [match1])
           (pprMatches sty print_info rest)
\end{code}

\begin{code}
instance (NamedThing bdee, Outputable bdee, 
             NamedThing pat, Outputable pat) =>
		Outputable (Match bdee pat) where
    ppr sty b	= panic "ppr: Match"
\end{code}

%************************************************************************
%*									*
\subsection[AbsSyntax-GRHSsAndBinds]{Guarded RHSs plus their Binds}
%*									*
%************************************************************************

Possibly \tr{NoGuardNoBinds{In,Out}}, etc.? ToDo

\begin{code}
data GRHSsAndBinds bdee pat
   = GRHSsAndBindsIn 	[GRHS bdee pat]	    -- at least one GRHS
			(Binds bdee pat)

   | GRHSsAndBindsOut 	[GRHS bdee pat]	    -- at least one GRHS
			(Binds bdee pat)
			UniType

type ProtoNameGRHSsAndBinds   = GRHSsAndBinds ProtoName ProtoNamePat
type RenamedGRHSsAndBinds     = GRHSsAndBinds Name  	RenamedPat
type TypecheckedGRHSsAndBinds = GRHSsAndBinds Id    	TypecheckedPat
\end{code}

\begin{code}
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
\end{code}

\begin{code}
instance (NamedThing bdee, Outputable bdee, 
             NamedThing pat, Outputable pat) =>
		Outputable (GRHSsAndBinds bdee pat) where
    ppr sty b = panic "ppr:GRHSsAndBinds"
\end{code}

%************************************************************************
%*									*
\subsection[AbsSyntax-GRHS]{A guarded right-hand-side}
%*									*
%************************************************************************

Sets of guarded right hand sides. In
\begin{verbatim}
f (x,y) | x==True = y
        | otherwise = y*2
\end{verbatim}
a guarded right hand side is either
@(x==True = y)@, or @(otherwise = y*2)@.

For each match, there may be several guarded right hand
sides, as the definition of @f@ shows.

\begin{code}
data GRHS bdee pat
  = GRHS	    (Expr bdee pat)	-- guard(ed)...
		    (Expr bdee pat)	-- ... right-hand side
		    SrcLoc

  | OtherwiseGRHS   (Expr bdee pat)	-- guard-free
		    SrcLoc
\end{code}

And, as always:
\begin{code}
type ProtoNameGRHS   = GRHS ProtoName ProtoNamePat
type RenamedGRHS     = GRHS Name      RenamedPat
type TypecheckedGRHS = GRHS Id        TypecheckedPat
\end{code}

\begin{code}
pprGRHS :: (NamedThing bdee, Outputable bdee, 
              NamedThing pat, Outputable pat) =>
		PprStyle -> Bool -> GRHS bdee pat -> Pretty

pprGRHS sty is_case (GRHS guard expr locn)
 = ppAboves [
	ifPprShowAll sty (ppr sty locn),
	ppHang (ppCat [ppStr "|", ppr sty guard, ppStr (if is_case then "->" else "=")])
		  4 (ppr sty expr)
   ]

pprGRHS sty is_case (OtherwiseGRHS  expr locn)
 = ppAboves [
	ifPprShowAll sty (ppr sty locn),
	ppHang (ppStr (if is_case then "->" else "="))
	  4 (ppr sty expr)
   ]
\end{code}

\begin{code}
instance (NamedThing bdee, Outputable bdee, 
            NamedThing pat, Outputable pat) =>
		Outputable (GRHS bdee pat) where
    ppr sty b	= panic "ppr: GRHSs"
\end{code}
