%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PatSyntax]{Abstract Haskell syntax---patterns}

\begin{code}
#include "HsVersions.h"

module HsPat (
	InPat(..),
	OutPat(..),

	irrefutablePat, irrefutablePats,
	failureFreePat,
	patsAreAllCons, isConPat,
	patsAreAllLits,	isLitPat,
	collectPatBinders
    ) where

IMP_Ubiq()

-- friends:
import HsBasic			( HsLit, Fixity )
IMPORT_DELOOPER(HsLoop)		( HsExpr )

-- others:
import Id		( dataConTyCon, GenId )
import Maybes		( maybeToBool )
import Name		( pprSym, pprNonSym )
import Outputable	( interppSP, interpp'SP, ifPprShowAll )
import PprStyle		( PprStyle(..) )
import Pretty
import TyCon		( maybeTyConSingleCon )
import PprType		( GenType )
\end{code}

Patterns come in distinct before- and after-typechecking flavo(u)rs.
\begin{code}
data InPat name
  = WildPatIn				-- wild card
  | VarPatIn	    name		-- variable
  | LitPatIn	    HsLit		-- literal
  | LazyPatIn	    (InPat name)	-- lazy pattern
  | AsPatIn	    name		-- as pattern
		    (InPat name)
  | ConPatIn	    name		-- constructed type
		    [InPat name]
  | ConOpPatIn	    (InPat name)
		    name
		    Fixity		-- c.f. OpApp in HsExpr
		    (InPat name)

  | NPlusKPatIn	    name		--  n+k pattern
		    HsLit

  -- We preserve prefix negation and parenthesis for the precedence parser.

  | NegPatIn	    (InPat name)	-- negated pattern
  | ParPatIn        (InPat name)	-- parenthesised pattern

  | ListPatIn	    [InPat name]	-- syntactic list
					-- must have >= 1 elements
  | TuplePatIn	    [InPat name]	-- tuple

  | RecPatIn	    name 		-- record
		    [(name, InPat name, Bool)]	-- True <=> source used punning

data OutPat tyvar uvar id
  = WildPat	    (GenType tyvar uvar)	-- wild card

  | VarPat	    id				-- variable (type is in the Id)

  | LazyPat	    (OutPat tyvar uvar id)	-- lazy pattern

  | AsPat	    id				-- as pattern
		    (OutPat tyvar uvar id)

  | ConPat	    Id				-- Constructor is always an Id
		    (GenType tyvar uvar)    	-- the type of the pattern
		    [OutPat tyvar uvar id]

  | ConOpPat	    (OutPat tyvar uvar id)	-- just a special case...
		    Id
		    (OutPat tyvar uvar id)
		    (GenType tyvar uvar)
  | ListPat		 	    		-- syntactic list
		    (GenType tyvar uvar)	-- the type of the elements
   	    	    [OutPat tyvar uvar id]

  | TuplePat	    [OutPat tyvar uvar id]	-- tuple
						-- UnitPat is TuplePat []

  | RecPat	    Id 				-- record constructor
		    (GenType tyvar uvar)    	-- the type of the pattern
		    [(Id, OutPat tyvar uvar id, Bool)]	-- True <=> source used punning

  | LitPat	    -- Used for *non-overloaded* literal patterns:
		    -- Int#, Char#, Int, Char, String, etc.
		    HsLit
		    (GenType tyvar uvar) 	-- type of pattern

  | NPat	    -- Used for *overloaded* literal patterns
		    HsLit			-- the literal is retained so that
						-- the desugarer can readily identify
						-- equations with identical literal-patterns
		    (GenType tyvar uvar) 	-- type of pattern, t
   	    	    (HsExpr tyvar uvar id (OutPat tyvar uvar id))
						-- of type t -> Bool; detects match

  | NPlusKPat	    id
		    HsLit			-- Same reason as for LitPat
						-- (This could be an Integer, but then
						-- it's harder to partitionEqnsByLit
						-- in the desugarer.)
		    (GenType tyvar uvar)    	-- Type of pattern, t
   	    	    (HsExpr tyvar uvar id (OutPat tyvar uvar id)) 	-- Of type t -> Bool; detects match
   	    	    (HsExpr tyvar uvar id (OutPat tyvar uvar id)) 	-- Of type t -> t; subtracts k

  | DictPat	    -- Used when destructing Dictionaries with an explicit case
		    [id]			-- superclass dicts
		    [id]			-- methods
\end{code}

\begin{code}
instance (Outputable name, NamedThing name) => Outputable (InPat name) where
    ppr = pprInPat

pprInPat :: (Outputable name, NamedThing name) => PprStyle -> InPat name -> Pretty

pprInPat sty (WildPatIn)	= ppChar '_'
pprInPat sty (VarPatIn var)	= ppr sty var
pprInPat sty (LitPatIn s)	= ppr sty s
pprInPat sty (LazyPatIn pat)	= ppBeside (ppChar '~') (ppr sty pat)
pprInPat sty (AsPatIn name pat)
    = ppBesides [ppLparen, ppr sty name, ppChar '@', ppr sty pat, ppRparen]

pprInPat sty (ConPatIn c pats)
 = if null pats then
      ppr sty c
   else
      ppCat [ppr sty c, interppSP sty pats] -- ParPats put in the parens

pprInPat sty (ConOpPatIn pat1 op fixity pat2)
 = ppCat [ppr sty pat1, ppr sty op, ppr sty pat2] -- ParPats put in parens

	-- ToDo: use pprSym to print op (but this involves fiddling various
	-- contexts & I'm lazy...); *PatIns are *rarely* printed anyway... (WDP)

pprInPat sty (NegPatIn pat)
  = let
	pp_pat = pprInPat sty pat
    in
    ppBeside (ppChar '-') (
    case pat of
      LitPatIn _ -> pp_pat
      _          -> ppParens pp_pat
    )

pprInPat sty (ParPatIn pat)
  = ppParens (pprInPat sty pat)

pprInPat sty (ListPatIn pats)
  = ppBesides [ppLbrack, interpp'SP sty pats, ppRbrack]
pprInPat sty (TuplePatIn pats)
  = ppParens (interpp'SP sty pats)
pprInPat sty (NPlusKPatIn n k)
  = ppBesides [ppLparen, ppr sty n, ppChar '+', ppr sty k, ppRparen]

pprInPat sty (RecPatIn con rpats)
  = ppCat [ppr sty con, ppCurlies (ppIntersperse pp'SP (map (pp_rpat sty) rpats))]
  where
    pp_rpat PprForUser (v, _, True) = ppr PprForUser v
    pp_rpat sty        (v, p, _)    = ppCat [ppr sty v, ppChar '=', ppr sty p]
\end{code}

\begin{code}
instance (Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar, Outputable id)
       => Outputable (OutPat tyvar uvar id) where
    ppr = pprOutPat
\end{code}

\begin{code}
pprOutPat sty (WildPat ty)	= ppChar '_'
pprOutPat sty (VarPat var)	= ppr sty var
pprOutPat sty (LazyPat pat)	= ppBesides [ppChar '~', ppr sty pat]
pprOutPat sty (AsPat name pat)
  = ppBesides [ppLparen, ppr sty name, ppChar '@', ppr sty pat, ppRparen]

pprOutPat sty (ConPat name ty [])
  = ppBeside (ppr sty name)
	(ifPprShowAll sty (pprConPatTy sty ty))

pprOutPat sty (ConPat name ty pats)
  = ppBesides [ppLparen, ppr sty name, ppSP,
    	 interppSP sty pats, ppRparen,
    	 ifPprShowAll sty (pprConPatTy sty ty) ]

pprOutPat sty (ConOpPat pat1 op pat2 ty)
  = ppBesides [ppLparen, ppr sty pat1, ppSP, pprSym sty op, ppSP, ppr sty pat2, ppRparen]

pprOutPat sty (ListPat ty pats)
  = ppBesides [ppLbrack, interpp'SP sty pats, ppRbrack]
pprOutPat sty (TuplePat pats)
  = ppParens (interpp'SP sty pats)

pprOutPat sty (RecPat con ty rpats)
  = ppBesides [ppr sty con, ppCurlies (ppIntersperse pp'SP (map (pp_rpat sty) rpats))]
  where
    pp_rpat PprForUser (v, _, True) = ppr PprForUser v
    pp_rpat sty (v, p, _)           = ppCat [ppr sty v, ppChar '=', ppr sty p]

pprOutPat sty (LitPat l ty) 	= ppr sty l	-- ToDo: print more
pprOutPat sty (NPat   l ty e)	= ppr sty l	-- ToDo: print more
pprOutPat sty (NPlusKPat n k ty e1 e2)		-- ToDo: print more
  = ppBesides [ppLparen, ppr sty n, ppChar '+', ppr sty k, ppRparen]

pprOutPat sty (DictPat dicts methods)
 = ppSep [ppBesides [ppLparen, ppPStr SLIT("{-dict-}")],
	  ppBracket (interpp'SP sty dicts),
	  ppBesides [ppBracket (interpp'SP sty methods), ppRparen]]

pprConPatTy sty ty
 = ppParens (ppr sty ty)
\end{code}

%************************************************************************
%*									*
%* predicates for checking things about pattern-lists in EquationInfo	*
%*									*
%************************************************************************
\subsection[Pat-list-predicates]{Look for interesting things in patterns}

Unlike in the Wadler chapter, where patterns are either ``variables''
or ``constructors,'' here we distinguish between:
\begin{description}
\item[unfailable:]
Patterns that cannot fail to match: variables, wildcards, and lazy
patterns.

These are the irrefutable patterns; the two other categories
are refutable patterns.

\item[constructor:]
A non-literal constructor pattern (see next category).

\item[literal patterns:]
At least the numeric ones may be overloaded.
\end{description}

A pattern is in {\em exactly one} of the above three categories; `as'
patterns are treated specially, of course.

The 1.3 report defines what ``irrefutable'' and ``failure-free'' patterns are.
\begin{code}
irrefutablePats :: [OutPat a b c] -> Bool
irrefutablePats pat_list = all irrefutablePat pat_list

irrefutablePat (AsPat	_ pat)	= irrefutablePat pat
irrefutablePat (WildPat	_)	= True
irrefutablePat (VarPat	_)	= True
irrefutablePat (LazyPat	_)	= True
irrefutablePat (DictPat ds ms)	= (length ds + length ms) <= 1
irrefutablePat other		= False

failureFreePat :: OutPat a b c -> Bool

failureFreePat (WildPat _) 		  = True
failureFreePat (VarPat _)  		  = True
failureFreePat (LazyPat	_) 		  = True
failureFreePat (AsPat _ pat)		  = failureFreePat pat
failureFreePat (ConPat con tys pats)	  = only_con con && all failureFreePat pats
failureFreePat (ConOpPat pat1 con pat2 _) = only_con con && failureFreePat pat1 && failureFreePat pat1
failureFreePat (RecPat con _ fields)	  = only_con con && and [ failureFreePat pat | (_,pat,_) <- fields ]
failureFreePat (ListPat _ _)		  = False
failureFreePat (TuplePat pats)		  = all failureFreePat pats
failureFreePat (DictPat _ _)		  = True
failureFreePat other_pat		  = False   -- Literals, NPat

only_con con = maybeToBool (maybeTyConSingleCon (dataConTyCon con))
\end{code}

\begin{code}
patsAreAllCons :: [OutPat a b c] -> Bool
patsAreAllCons pat_list = all isConPat pat_list

isConPat (AsPat _ pat)		= isConPat pat
isConPat (ConPat _ _ _)		= True
isConPat (ConOpPat _ _ _ _)	= True
isConPat (ListPat _ _)		= True
isConPat (TuplePat _)		= True
isConPat (RecPat _ _ _)		= True
isConPat (DictPat ds ms)	= (length ds + length ms) > 1
isConPat other			= False

patsAreAllLits :: [OutPat a b c] -> Bool
patsAreAllLits pat_list = all isLitPat pat_list

isLitPat (AsPat _ pat)	       = isLitPat pat
isLitPat (LitPat _ _)	       = True
isLitPat (NPat   _ _ _)	       = True
isLitPat (NPlusKPat _ _ _ _ _) = True
isLitPat other		       = False
\end{code}

This function @collectPatBinders@ works with the ``collectBinders''
functions for @HsBinds@, etc.  The order in which the binders are
collected is important; see @HsBinds.lhs@.
\begin{code}
collectPatBinders :: InPat a -> [a]

collectPatBinders WildPatIn	      	 = []
collectPatBinders (VarPatIn var)      	 = [var]
collectPatBinders (LitPatIn _)	      	 = []
collectPatBinders (LazyPatIn pat)     	 = collectPatBinders pat
collectPatBinders (AsPatIn a pat)     	 = a : collectPatBinders pat
collectPatBinders (NPlusKPatIn n _)      = [n]
collectPatBinders (ConPatIn c pats)   	 = concat (map collectPatBinders pats)
collectPatBinders (ConOpPatIn p1 c f p2) = collectPatBinders p1 ++ collectPatBinders p2
collectPatBinders (NegPatIn  pat)     	 = collectPatBinders pat
collectPatBinders (ParPatIn  pat)     	 = collectPatBinders pat
collectPatBinders (ListPatIn pats)    	 = concat (map collectPatBinders pats)
collectPatBinders (TuplePatIn pats)   	 = concat (map collectPatBinders pats)
collectPatBinders (RecPatIn c fields) 	 = concat (map (\ (f,pat,_) -> collectPatBinders pat) fields)
\end{code}
