%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PatSyntax]{Abstract Haskell syntax---patterns}

\begin{code}
module HsPat (
	InPat(..),
	OutPat(..),

	irrefutablePat, irrefutablePats,
	failureFreePat,
	patsAreAllCons, isConPat,
	patsAreAllLits,	isLitPat,
	collectPatBinders
    ) where

#include "HsVersions.h"

-- friends:
import HsBasic		( HsLit )
import HsExpr		( HsExpr )
import BasicTypes	( Fixity )

-- others:
import Var		( Id, GenTyVar )
import DataCon		( DataCon, dataConTyCon )
import Maybes		( maybeToBool )
import Outputable	
import TyCon		( maybeTyConSingleCon )
import Type		( GenType )
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
  | TuplePatIn	    [InPat name] Bool	-- tuple (boxed?)

  | RecPatIn	    name 		-- record
		    [(name, InPat name, Bool)]	-- True <=> source used punning

data OutPat flexi id
  = WildPat	    (GenType flexi)	-- wild card

  | VarPat	    id				-- variable (type is in the Id)

  | LazyPat	    (OutPat flexi id)	-- lazy pattern

  | AsPat	    id				-- as pattern
		    (OutPat flexi id)

  | ListPat		 	    		-- syntactic list
		    (GenType flexi)	-- the type of the elements
   	    	    [OutPat flexi id]

  | TuplePat	    [OutPat flexi id]	-- tuple
		    Bool		-- boxed?
						-- UnitPat is TuplePat []

  | ConPat	    DataCon
		    (GenType flexi)    	-- the type of the pattern
		    [GenTyVar flexi]	-- Existentially bound type variables
		    [id]		-- Ditto dictionaries
		    [OutPat flexi id]

  -- ConOpPats are only used on the input side

  | RecPat	    DataCon		-- record constructor
		    (GenType flexi)    	-- the type of the pattern
		    [GenTyVar flexi]	-- Existentially bound type variables
		    [id]		-- Ditto dictionaries
		    [(Id, OutPat flexi id, Bool)]	-- True <=> source used punning

  | LitPat	    -- Used for *non-overloaded* literal patterns:
		    -- Int#, Char#, Int, Char, String, etc.
		    HsLit
		    (GenType flexi) 	-- type of pattern

  | NPat	    -- Used for *overloaded* literal patterns
		    HsLit			-- the literal is retained so that
						-- the desugarer can readily identify
						-- equations with identical literal-patterns
		    (GenType flexi) 	-- type of pattern, t
   	    	    (HsExpr flexi id (OutPat flexi id))
						-- of type t -> Bool; detects match

  | NPlusKPat	    id
		    HsLit			-- Same reason as for LitPat
						-- (This could be an Integer, but then
						-- it's harder to partitionEqnsByLit
						-- in the desugarer.)
		    (GenType flexi)    	-- Type of pattern, t
   	    	    (HsExpr flexi id (OutPat flexi id)) 	-- Of type t -> Bool; detects match
   	    	    (HsExpr flexi id (OutPat flexi id)) 	-- Of type t -> t; subtracts k

  | DictPat	    -- Used when destructing Dictionaries with an explicit case
		    [id]			-- superclass dicts
		    [id]			-- methods
\end{code}

Now name in Inpat is not need to be in NAmedThing to be Outputable.
Needed by ../deSugar/Check.lhs

JJQC-2-12-97

\begin{code}
instance (Outputable name) => Outputable (InPat name) where
    ppr = pprInPat

pprInPat :: (Outputable name) => InPat name -> SDoc

pprInPat (WildPatIn)	    = char '_'
pprInPat (VarPatIn var)	    = ppr var
pprInPat (LitPatIn s)	    = ppr s
pprInPat (LazyPatIn pat)    = char '~' <> ppr pat
pprInPat (AsPatIn name pat) = parens (hcat [ppr name, char '@', ppr pat])

pprInPat (ConPatIn c pats)
  | null pats = ppr c
  | otherwise = hsep [ppr c, interppSP pats] -- ParPats put in the parens

pprInPat (ConOpPatIn pat1 op fixity pat2)
 = hsep [ppr pat1, ppr op, ppr pat2] -- ParPats put in parens

	-- ToDo: use pprSym to print op (but this involves fiddling various
	-- contexts & I'm lazy...); *PatIns are *rarely* printed anyway... (WDP)

pprInPat (NegPatIn pat)
  = let
	pp_pat = pprInPat pat
    in
    char '-' <> (
    case pat of
      LitPatIn _ -> pp_pat
      _          -> parens pp_pat
    )

pprInPat (ParPatIn pat)
  = parens (pprInPat pat)

pprInPat (ListPatIn pats)
  = brackets (interpp'SP pats)
pprInPat (TuplePatIn pats False)
  = text "(#" <> (interpp'SP pats) <> text "#)"
pprInPat (TuplePatIn pats True)
  = parens (interpp'SP pats)
pprInPat (NPlusKPatIn n k)
  = parens (hcat [ppr n, char '+', ppr k])

pprInPat (RecPatIn con rpats)
  = hsep [ppr con, braces (hsep (punctuate comma (map (pp_rpat) rpats)))]
  where
    pp_rpat (v, _, True) = ppr v
    pp_rpat (v, p, _)    = hsep [ppr v, char '=', ppr p]
\end{code}

\begin{code}
instance (Outputable id) => Outputable (OutPat flexi id) where
    ppr = pprOutPat
\end{code}

\begin{code}
pprOutPat (WildPat ty)	= char '_'
pprOutPat (VarPat var)	= ppr var
pprOutPat (LazyPat pat)	= hcat [char '~', ppr pat]
pprOutPat (AsPat name pat)
  = parens (hcat [ppr name, char '@', ppr pat])

pprOutPat (ConPat name ty [] [] [])
  = ppr name

pprOutPat (ConPat name ty tyvars dicts pats)
  = parens (hsep [ppr name, interppSP tyvars, interppSP dicts, interppSP pats])

pprOutPat (ListPat ty pats)
  = brackets (interpp'SP pats)
pprOutPat (TuplePat pats boxed@True)
  = parens (interpp'SP pats)
pprOutPat (TuplePat pats unboxed@False)
  = text "(#" <> (interpp'SP pats) <> text "#)"

pprOutPat (RecPat con ty tvs dicts rpats)
  = hsep [ppr con, interppSP tvs, interppSP dicts, braces (hsep (punctuate comma (map (pp_rpat) rpats)))]
  where
    pp_rpat (v, _, True) = ppr v
    pp_rpat (v, p, _)    = hsep [ppr v, char '=', ppr p]

pprOutPat (LitPat l ty) 	= ppr l	-- ToDo: print more
pprOutPat (NPat   l ty e)	= ppr l	-- ToDo: print more
pprOutPat (NPlusKPat n k ty e1 e2)		-- ToDo: print more
  = parens (hcat [ppr n, char '+', ppr k])

pprOutPat (DictPat dicts methods)
 = parens (sep [ptext SLIT("{-dict-}"),
		  brackets (interpp'SP dicts),
		  brackets (interpp'SP methods)])

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
irrefutablePats :: [OutPat a b] -> Bool
irrefutablePats pat_list = all irrefutablePat pat_list

irrefutablePat (AsPat	_ pat)	= irrefutablePat pat
irrefutablePat (WildPat	_)	= True
irrefutablePat (VarPat	_)	= True
irrefutablePat (LazyPat	_)	= True
irrefutablePat (DictPat ds ms)	= (length ds + length ms) <= 1
irrefutablePat other		= False

failureFreePat :: OutPat a b -> Bool

failureFreePat (WildPat _) 		  = True
failureFreePat (VarPat _)  		  = True
failureFreePat (LazyPat	_) 		  = True
failureFreePat (AsPat _ pat)		  = failureFreePat pat
failureFreePat (ConPat con tys _ _ pats)  = only_con con && all failureFreePat pats
failureFreePat (RecPat con _ _ _ fields)  = only_con con && and [ failureFreePat pat | (_,pat,_) <- fields ]
failureFreePat (ListPat _ _)		  = False
failureFreePat (TuplePat pats _)	  = all failureFreePat pats
failureFreePat (DictPat _ _)		  = True
failureFreePat other_pat		  = False   -- Literals, NPat

only_con con = maybeToBool (maybeTyConSingleCon (dataConTyCon con))
\end{code}

\begin{code}
patsAreAllCons :: [OutPat a b] -> Bool
patsAreAllCons pat_list = all isConPat pat_list

isConPat (AsPat _ pat)		= isConPat pat
isConPat (ConPat _ _ _ _ _)	= True
isConPat (ListPat _ _)		= True
isConPat (TuplePat _ _)		= True
isConPat (RecPat _ _ _ _ _)	= True
isConPat (DictPat ds ms)	= (length ds + length ms) > 1
isConPat other			= False

patsAreAllLits :: [OutPat a b] -> Bool
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
collectPatBinders (TuplePatIn pats _)  	 = concat (map collectPatBinders pats)
collectPatBinders (RecPatIn c fields) 	 = concat (map (\ (f,pat,_) -> collectPatBinders pat) fields)
\end{code}
