%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[PatSyntax]{Abstract Haskell syntax---patterns}

\begin{code}
#include "HsVersions.h"

module HsPat where

import AbsPrel		( mkTupleTy, mkListTy
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
#ifdef DPH
			  , mkProcessorTy
#endif 
			)
import AbsUniType
import HsLit		( Literal )
import HsExpr		( Expr, TypecheckedExpr(..) )
import Id
import IdInfo
import Maybes		( maybeToBool, Maybe(..) )
import Name		( Name )
import ProtoName	( ProtoName(..) ) -- .. for pragmas only
import Outputable
import Pretty
import Unique		( Unique )
import Util
\end{code}

Patterns come in distinct before- and after-typechecking flavo(u)rs.
\begin{code}
data InPat name
  = WildPatIn				--X wild card
  | VarPatIn	    name		--X variable
  | LitPatIn	    Literal		--  literal
  | LazyPatIn	    (InPat name)	--X lazy pattern
  | AsPatIn	    name		--X as pattern
		    (InPat name)
  | ConPatIn	    name		--X constructed type
		    [(InPat name)]
  | ConOpPatIn	    (InPat name)
		    name
		    (InPat name)
  | ListPatIn	    [InPat name]		--X syntactic list
					-- must have >= 1 elements
  | TuplePatIn	    [InPat name]		--X tuple
					-- UnitPat is TuplePat []
  | NPlusKPatIn	    name		--  n+k pattern
		    Literal
#ifdef DPH
  | ProcessorPatIn  [(InPat name)] 
		     (InPat name)	-- (|pat1,...,patK;pat|)
#endif {- Data Parallel Haskell -}

type ProtoNamePat = InPat ProtoName
type RenamedPat = InPat Name

data TypecheckedPat
  = WildPat	    UniType 	    	-- wild card

  | VarPat	    Id			-- variable (type is in the Id)

  | LazyPat	    TypecheckedPat	-- lazy pattern

  | AsPat	    Id		-- as pattern
		    TypecheckedPat

  | ConPat	    Id		-- constructed type;
		    UniType    		-- the type of the pattern
		    [TypecheckedPat]

  | ConOpPat	    TypecheckedPat	-- just a special case...
		    Id
		    TypecheckedPat
		    UniType
  | ListPat		 	    	-- syntactic list
		    UniType		-- the type of the elements
   	    	    [TypecheckedPat]

  | TuplePat	    [TypecheckedPat]	-- tuple
					-- UnitPat is TuplePat []

  | LitPat	    -- Used for *non-overloaded* literal patterns:
		    -- Int#, Char#, Int, Char, String, etc.
		    Literal
		    UniType 	    	-- type of pattern

  | NPat	    -- Used for *overloaded* literal patterns
		    Literal		-- the literal is retained so that
					-- the desugarer can readily identify
					-- equations with identical literal-patterns
		    UniType 	    	-- type of pattern, t
   	    	    TypecheckedExpr 	-- Of type t -> Bool; detects match

  | NPlusKPat	    Id
		    Literal		-- Same reason as for LitPat
					-- (This could be an Integer, but then
					-- it's harder to partitionEqnsByLit
					-- in the desugarer.)
		    UniType 	    	-- Type of pattern, t
		    TypecheckedExpr	-- "fromInteger literal"; of type t
   	    	    TypecheckedExpr 	-- Of type t-> t -> Bool; detects match
   	    	    TypecheckedExpr 	-- Of type t -> t -> t; subtracts k
#ifdef DPH
  | ProcessorPat   
		    [TypecheckedPat]	-- Typechecked Pattern 
		    [TypecheckedExpr]	-- Of type t-> Integer; conversion
		    TypecheckedPat	-- Data at that processor
#endif {- Data Parallel Haskell -}
\end{code}

Note: If @typeOfPat@ doesn't bear a strong resemblance to @typeOfCoreExpr@,
then something is wrong.
\begin{code}
typeOfPat :: TypecheckedPat -> UniType
typeOfPat (WildPat ty)		= ty
typeOfPat (VarPat var)		= getIdUniType var
typeOfPat (LazyPat pat)		= typeOfPat pat
typeOfPat (AsPat var pat)	= getIdUniType var
typeOfPat (ConPat _ ty _)	= ty
typeOfPat (ConOpPat _ _ _ ty)	= ty
typeOfPat (ListPat ty _)	= mkListTy ty
typeOfPat (TuplePat pats)	= mkTupleTy (length pats) (map typeOfPat pats)
typeOfPat (LitPat lit ty)	= ty
typeOfPat (NPat	  lit ty _)	= ty
typeOfPat (NPlusKPat n k ty _ _ _) = ty
#ifdef DPH
-- Should be more efficient to find type of pid than pats 
typeOfPat (ProcessorPat pats _ pat) 
   = mkProcessorTy (map typeOfPat pats) (typeOfPat pat)
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
instance (NamedThing name) => NamedThing (InPat name) where
    hasType pat		= False
#ifdef DEBUG
    getExportFlag 	= panic "NamedThing.InPat.getExportFlag"
    isLocallyDefined	= panic "NamedThing.InPat.isLocallyDefined"
    getOrigName		= panic "NamedThing.InPat.getOrigName"
    getOccurrenceName	= panic "NamedThing.InPat.getOccurrenceName"
    getInformingModules	= panic "NamedThing.InPat.getOccurrenceName"
    getSrcLoc		= panic "NamedThing.InPat.getSrcLoc"
    getTheUnique	= panic "NamedThing.InPat.getTheUnique"
    getType pat		= panic "NamedThing.InPat.getType"
    fromPreludeCore	= panic "NamedThing.InPat.fromPreludeCore"
#endif

instance NamedThing TypecheckedPat where
    hasType pat		= True
    getType		= typeOfPat
#ifdef DEBUG
    getExportFlag 	= panic "NamedThing.TypecheckedPat.getExportFlag"
    isLocallyDefined	= panic "NamedThing.TypecheckedPat.isLocallyDefined"
    getOrigName		= panic "NamedThing.TypecheckedPat.getOrigName"
    getOccurrenceName	= panic "NamedThing.TypecheckedPat.getOccurrenceName"
    getInformingModules	= panic "NamedThing.TypecheckedPat.getOccurrenceName"
    getSrcLoc		= panic "NamedThing.TypecheckedPat.getSrcLoc"
    getTheUnique	= panic "NamedThing.TypecheckedPat.getTheUnique"
    fromPreludeCore	= panic "NamedThing.TypecheckedPat.fromPreludeCore"
#endif
\end{code}

\begin{code}
instance (Outputable name) => Outputable (InPat name) where
    ppr = pprInPat

pprInPat :: (Outputable name) => PprStyle -> InPat name -> Pretty
pprInPat sty (WildPatIn)	= ppStr "_"
pprInPat sty (VarPatIn var)	= ppr sty var
pprInPat sty (LitPatIn s)	= ppr sty s
pprInPat sty (LazyPatIn pat)	= ppBeside (ppChar '~') (ppr sty pat)
pprInPat sty (AsPatIn name pat)
    = ppBesides [ppLparen, ppr sty name, ppChar '@', ppr sty pat, ppRparen]

pprInPat sty (ConPatIn c pats)
 = if null pats then
      ppr sty c
   else
      ppBesides [ppLparen, ppr sty c, ppSP, interppSP sty pats, ppRparen]


pprInPat sty (ConOpPatIn pat1 op pat2)
 = ppBesides [ppLparen, ppr sty pat1, ppSP, ppr sty op, ppSP, ppr sty pat2, ppRparen]

-- ToDo: use pprOp to print op (but this involves fiddling various
-- contexts & I'm lazy...); *PatIns are *rarely* printed anyway... (WDP)

pprInPat sty (ListPatIn pats)
  = ppBesides [ppLbrack, interpp'SP sty pats, ppRbrack]
pprInPat sty (TuplePatIn pats)
  = ppBesides [ppLparen, interpp'SP sty pats, ppRparen]
pprInPat sty (NPlusKPatIn n k)
  = ppBesides [ppLparen, ppr sty n, ppChar '+', ppr sty k, ppRparen]
#ifdef DPH
pprInPat sty (ProcessorPatIn pats pat)
      = ppBesides [ppStr "(|", interpp'SP sty pats,ppSemi ,
		   ppr sty pat , ppStr "|)"]
#endif {- Data Parallel Haskell -}
\end{code}

Problems with @Outputable@ instance for @TypecheckedPat@ when no
original names.
\begin{code}
instance Outputable TypecheckedPat where
    ppr = pprTypecheckedPat
\end{code}

\begin{code}
pprTypecheckedPat sty (WildPat ty)	= ppChar '_'
pprTypecheckedPat sty (VarPat var)	= ppr sty var
pprTypecheckedPat sty (LazyPat pat)	= ppBesides [ppChar '~', ppr sty pat]
pprTypecheckedPat sty (AsPat name pat)
  = ppBesides [ppLparen, ppr sty name, ppChar '@', ppr sty pat, ppRparen]

pprTypecheckedPat sty (ConPat name ty [])
  = ppBeside (ppr sty name)
	(ifPprShowAll sty (pprConPatTy sty ty))

pprTypecheckedPat sty (ConPat name ty pats)
  = ppBesides [ppLparen, ppr sty name, ppSP,
    	 interppSP sty pats, ppRparen,
    	 ifPprShowAll sty (pprConPatTy sty ty) ]

pprTypecheckedPat sty (ConOpPat pat1 op pat2 ty)
  = ppBesides [ppLparen, ppr sty pat1, ppSP, pprOp sty op, ppSP, ppr sty pat2, ppRparen]

pprTypecheckedPat sty (ListPat ty pats)
  = ppBesides [ppLbrack, interpp'SP sty pats, ppRbrack]
pprTypecheckedPat sty (TuplePat pats)
  = ppBesides [ppLparen, interpp'SP sty pats, ppRparen]

pprTypecheckedPat sty (LitPat l ty) 	= ppr sty l	-- ToDo: print more
pprTypecheckedPat sty (NPat   l ty e)	= ppr sty l	-- ToDo: print more

pprTypecheckedPat sty (NPlusKPat n k ty e1 e2 e3)
  = case sty of
      PprForUser -> basic_ppr
      _		 -> ppHang basic_ppr 4 exprs_ppr
  where
    basic_ppr = ppBesides [ppLparen, ppr sty n, ppChar '+', ppr sty k, ppRparen]
    exprs_ppr = ppSep [ ppBeside (ppStr "{- ") (ppr sty ty),
			ppr sty e1, ppr sty e2,
			ppBeside (ppr sty e3) (ppStr " -}")]
#ifdef DPH
pprTypecheckedPat sty (ProcessorPat pats convs pat)
   = case sty of
      PprForUser -> basic_ppr
      _		 -> ppHang basic_ppr 4 exprs_ppr
  where
    basic_ppr = ppBesides [ppStr "(|", interpp'SP sty pats,ppSemi ,
		   	   ppr sty pat , ppStr "|)"]
    exprs_ppr = ppBesides [ppStr "{- " ,
			   ppr sty convs,
			   ppStr " -}"]
#endif {- Data Parallel Haskell -}

pprConPatTy :: PprStyle -> UniType -> Pretty
pprConPatTy sty ty
 = ppBesides [ppLparen, ppr sty ty, ppRparen]
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

\item[literal (including n+k patterns):]
At least the numeric ones may be overloaded.
\end{description}

A pattern is in {\em exactly one} of the above three categories; `as'
patterns are treated specially, of course.

\begin{code}
unfailablePats :: [TypecheckedPat] -> Bool
unfailablePats pat_list = all unfailablePat pat_list

unfailablePat (AsPat	_ pat)	= unfailablePat pat
unfailablePat (WildPat	_)	= True
unfailablePat (VarPat	_)	= True
unfailablePat (LazyPat	_)	= True
unfailablePat other		= False

patsAreAllCons :: [TypecheckedPat] -> Bool
patsAreAllCons pat_list = all isConPat pat_list

isConPat (AsPat _ pat)		= isConPat pat
isConPat (ConPat _ _ _)		= True
isConPat (ConOpPat _ _ _ _)	= True
isConPat (ListPat _ _)		= True
isConPat (TuplePat _)		= True
#ifdef DPH
isConPat (ProcessorPat _ _ _)	= True

#endif {- Data Parallel Haskell -}
isConPat other			= False

patsAreAllLits :: [TypecheckedPat] -> Bool
patsAreAllLits pat_list = all isLitPat pat_list

isLitPat (AsPat _ pat)		= isLitPat pat
isLitPat (LitPat _ _)		= True
isLitPat (NPat   _ _ _)		= True
isLitPat (NPlusKPat _ _ _ _ _ _)= True
isLitPat other			= False

#ifdef DPH
patsAreAllProcessor :: [TypecheckedPat] -> Bool
patsAreAllProcessor pat_list = all isProcessorPat pat_list
   where
      isProcessorPat (ProcessorPat _ _ _) = True
      isProcessorPat _			  = False
#endif 
\end{code}

\begin{code}
-- A pattern is irrefutable if a match on it cannot fail
-- (at any depth)
irrefutablePat :: TypecheckedPat -> Bool

irrefutablePat (WildPat _) 		  = True
irrefutablePat (VarPat _)  		  = True
irrefutablePat (LazyPat	_) 		  = True
irrefutablePat (AsPat _ pat)		  = irrefutablePat pat
irrefutablePat (ConPat con tys pats)	  = all irrefutablePat pats && only_con con
irrefutablePat (ConOpPat pat1 con pat2 _) = irrefutablePat pat1 && irrefutablePat pat1 && only_con con
irrefutablePat (ListPat _ _)		  = False
irrefutablePat (TuplePat pats)		  = all irrefutablePat pats
irrefutablePat other_pat		  = False	-- Literals, NPlusK, NPat

only_con con = maybeToBool (maybeSingleConstructorTyCon tycon)
 	       where
		 (_,_,_, tycon) = getDataConSig con
\end{code}
