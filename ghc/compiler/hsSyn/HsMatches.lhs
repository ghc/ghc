%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsMatches]{Abstract syntax: matches and guarded right-hand-sides}

The @Match@, @GRHSs@ and @GRHS@ datatypes.

\begin{code}
module HsMatches where

#include "HsVersions.h"

-- Friends
import HsExpr		( HsExpr, Stmt(..) )
import HsBinds		( HsBinds(..), nullBinds )
import HsTypes		( HsTyVarBndr, HsType )

-- Others
import Type		( Type )
import SrcLoc		( SrcLoc )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{@Match@, @GRHSs@, and @GRHS@ datatypes}
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
same number of @pats@s inside it.  This corresponds to saying that
a function defined by pattern matching must have the same number of
patterns in each equation.

\begin{code}
data Match id pat
  = Match
	[HsTyVarBndr id] 			-- Tyvars wrt which this match is universally quantified
					-- 	emtpy after typechecking
	[pat]				-- The patterns
	(Maybe (HsType id))		-- A type signature for the result of the match
					--	Nothing after typechecking

	(GRHSs id pat)

-- GRHSs are used both for pattern bindings and for Matches
data GRHSs id pat	
  = GRHSs [GRHS id pat]		-- Guarded RHSs
	  (HsBinds id pat)	-- The where clause
	  (Maybe Type)		-- Just rhs_ty after type checking

data GRHS id pat
  = GRHS  [Stmt id pat]		-- The RHS is the final ExprStmt
				-- I considered using a RetunStmt, but
				-- it printed 'wrong' in error messages 
	  SrcLoc

mkSimpleMatch :: [pat] -> HsExpr id pat -> Maybe Type -> SrcLoc -> Match id pat
mkSimpleMatch pats rhs maybe_rhs_ty locn
  = Match [] pats Nothing (GRHSs (unguardedRHS rhs locn) EmptyBinds maybe_rhs_ty)

unguardedRHS :: HsExpr id pat -> SrcLoc -> [GRHS id pat]
unguardedRHS rhs loc = [GRHS [ExprStmt rhs loc] loc]
\end{code}

@getMatchLoc@ takes a @Match@ and returns the
source-location gotten from the GRHS inside.
THis is something of a nuisance, but no more.

\begin{code}
getMatchLoc :: Match id pat -> SrcLoc
getMatchLoc (Match _ _ _ (GRHSs (GRHS _ loc : _) _ _)) = loc
\end{code}

%************************************************************************
%*									*
\subsection{Printing}
%*									*
%************************************************************************

We know the list must have at least one @Match@ in it.
\begin{code}
pprMatches :: (Outputable id, Outputable pat)
	   => (Bool, SDoc) -> [Match id pat] -> SDoc
pprMatches print_info matches = vcat (map (pprMatch print_info) matches)


pprMatch :: (Outputable id, Outputable pat)
	   => (Bool, SDoc) -> Match id pat -> SDoc
pprMatch print_info@(is_case, name) (Match _ pats maybe_ty grhss)
  = maybe_name <+> sep [sep (map ppr pats), 
			ppr_maybe_ty,
			nest 2 (pprGRHSs is_case grhss)]
  where
    maybe_name | is_case   = empty
	       | otherwise = name
    ppr_maybe_ty = case maybe_ty of
			Just ty -> dcolon <+> ppr ty
			Nothing -> empty


pprGRHSs :: (Outputable id, Outputable pat)
	 => Bool -> GRHSs id pat -> SDoc
pprGRHSs is_case (GRHSs grhss binds maybe_ty)
  = vcat (map (pprGRHS is_case) grhss)
    $$
    (if nullBinds binds then empty
     else text "where" $$ nest 4 (pprDeeper (ppr binds)))


pprGRHS :: (Outputable id, Outputable pat)
	=> Bool -> GRHS id pat -> SDoc

pprGRHS is_case (GRHS [ExprStmt expr _] locn)
 =  text (if is_case then "->" else "=") <+> pprDeeper (ppr expr)

pprGRHS is_case (GRHS guarded locn)
 = sep [char '|' <+> interpp'SP guards,
	text (if is_case then "->" else "=") <+> pprDeeper (ppr expr)
   ]
 where
    ExprStmt expr _ = last guarded	-- Last stmt should be a ExprStmt for guards
    guards	    = init guarded
\end{code}
