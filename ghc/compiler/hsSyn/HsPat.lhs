%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PatSyntax]{Abstract Haskell syntax---patterns}

\begin{code}
module HsPat (
	InPat(..),
	OutPat(..),

	irrefutablePat, irrefutablePats,
	failureFreePat, isWildPat,
	patsAreAllCons, isConPat,
	patsAreAllLits,	isLitPat,
	collectPatBinders, collectPatsBinders
    ) where

#include "HsVersions.h"

-- friends:
import HsBasic		( HsLit )
import HsExpr		( HsExpr )
import HsTypes		( HsType )
import BasicTypes	( Fixity, Boxity, tupleParens )

-- others:
import Var		( Id, TyVar )
import DataCon		( DataCon, dataConTyCon )
import Name		( isDataSymOcc, getOccName, NamedThing )
import Maybes		( maybeToBool )
import Outputable	
import TyCon		( maybeTyConSingleCon )
import Type		( Type )
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
  | SigPatIn	    (InPat name)
		    (HsType name)
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
  | TuplePatIn	    [InPat name] Boxity	-- tuple (boxed?)

  | RecPatIn	    name 		-- record
		    [(name, InPat name, Bool)]	-- True <=> source used punning

data OutPat id
  = WildPat	    Type	-- wild card
  | VarPat	    id		-- variable (type is in the Id)
  | LazyPat	    (OutPat id)	-- lazy pattern
  | AsPat	    id		-- as pattern
		    (OutPat id)

  | ListPat		 	-- syntactic list
		    Type	-- the type of the elements
   	    	    [OutPat id]

  | TuplePat	    [OutPat id]	-- tuple
		    Boxity
						-- UnitPat is TuplePat []

  | ConPat	    DataCon
		    Type    	-- the type of the pattern
		    [TyVar]	-- Existentially bound type variables
		    [id]	-- Ditto dictionaries
		    [OutPat id]

  -- ConOpPats are only used on the input side

  | RecPat	    DataCon		-- record constructor
		    Type    	-- the type of the pattern
		    [TyVar]	-- Existentially bound type variables
		    [id]		-- Ditto dictionaries
		    [(Id, OutPat id, Bool)]	-- True <=> source used punning

  | LitPat	    -- Used for *non-overloaded* literal patterns:
		    -- Int#, Char#, Int, Char, String, etc.
		    HsLit
		    Type 	-- type of pattern

  | NPat	    -- Used for *overloaded* literal patterns
		    HsLit			-- the literal is retained so that
						-- the desugarer can readily identify
						-- equations with identical literal-patterns
		    Type 	-- type of pattern, t
   	    	    (HsExpr id (OutPat id))
						-- of type t -> Bool; detects match

  | NPlusKPat	    id
		    HsLit			-- Same reason as for LitPat
						-- (This could be an Integer, but then
						-- it's harder to partitionEqnsByLit
						-- in the desugarer.)
		    Type    	-- Type of pattern, t
   	    	    (HsExpr id (OutPat id)) 	-- Of type t -> Bool; detects match
   	    	    (HsExpr id (OutPat id)) 	-- Of type t -> t; subtracts k

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
pprInPat (SigPatIn pat ty)  = ppr pat <+> dcolon <+> ppr ty
pprInPat (LazyPatIn pat)    = char '~' <> ppr pat
pprInPat (AsPatIn name pat) = parens (hcat [ppr name, char '@', ppr pat])

pprInPat (ConPatIn c pats)
  | null pats = ppr c
  | otherwise = hsep [ppr c, interppSP pats] -- inner ParPats supply the necessary parens.

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
pprInPat (TuplePatIn pats boxity)
  = tupleParens boxity (interpp'SP pats)
pprInPat (NPlusKPatIn n k)
  = parens (hcat [ppr n, char '+', ppr k])

pprInPat (RecPatIn con rpats)
  = hsep [ppr con, braces (hsep (punctuate comma (map (pp_rpat) rpats)))]
  where
    pp_rpat (v, _, True) = ppr v
    pp_rpat (v, p, _)    = hsep [ppr v, char '=', ppr p]
\end{code}

\begin{code}
instance (NamedThing id, Outputable id) => Outputable (OutPat id) where
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

-- Kludge to get infix constructors to come out right
-- when ppr'ing desugar warnings.
pprOutPat (ConPat name ty tyvars dicts pats)
  = getPprStyle $ \ sty ->
    parens      $
    case pats of
      [p1,p2] 
        | userStyle sty && isDataSymOcc (getOccName name) ->
	    hsep [ppr p1, ppr name, ppr p2]
      _ -> hsep [ppr name, interppSP tyvars, interppSP dicts, interppSP pats]

pprOutPat (ListPat ty pats)      = brackets (interpp'SP pats)
pprOutPat (TuplePat pats boxity) = tupleParens boxity (interpp'SP pats)

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
irrefutablePats :: [OutPat id] -> Bool
irrefutablePats pat_list = all irrefutablePat pat_list

irrefutablePat (AsPat	_ pat)	= irrefutablePat pat
irrefutablePat (WildPat	_)	= True
irrefutablePat (VarPat	_)	= True
irrefutablePat (LazyPat	_)	= True
irrefutablePat (DictPat ds ms)	= (length ds + length ms) <= 1
irrefutablePat other		= False

failureFreePat :: OutPat id -> Bool

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
isWildPat (WildPat _) = True
isWildPat other	      = False

patsAreAllCons :: [OutPat id] -> Bool
patsAreAllCons pat_list = all isConPat pat_list

isConPat (AsPat _ pat)		= isConPat pat
isConPat (ConPat _ _ _ _ _)	= True
isConPat (ListPat _ _)		= True
isConPat (TuplePat _ _)		= True
isConPat (RecPat _ _ _ _ _)	= True
isConPat (DictPat ds ms)	= (length ds + length ms) > 1
isConPat other			= False

patsAreAllLits :: [OutPat id] -> Bool
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
collectPatBinders pat = collect pat []

collectPatsBinders :: [InPat a] -> [a]
collectPatsBinders pats = foldr collect [] pats

collect WildPatIn	      	 bndrs = bndrs
collect (VarPatIn var)      	 bndrs = var : bndrs
collect (LitPatIn _)	      	 bndrs = bndrs
collect (SigPatIn pat _)	 bndrs = collect pat bndrs
collect (LazyPatIn pat)     	 bndrs = collect pat bndrs
collect (AsPatIn a pat)     	 bndrs = a : collect pat bndrs
collect (NPlusKPatIn n _)        bndrs = n : bndrs
collect (ConPatIn c pats)   	 bndrs = foldr collect bndrs pats
collect (ConOpPatIn p1 c f p2)   bndrs = collect p1 (collect p2 bndrs)
collect (NegPatIn  pat)     	 bndrs = collect pat bndrs
collect (ParPatIn  pat)     	 bndrs = collect pat bndrs
collect (ListPatIn pats)    	 bndrs = foldr collect bndrs pats
collect (TuplePatIn pats _)  	 bndrs = foldr collect bndrs pats
collect (RecPatIn c fields) 	 bndrs = foldr (\ (f,pat,_) bndrs -> collect pat bndrs) bndrs fields
\end{code}
