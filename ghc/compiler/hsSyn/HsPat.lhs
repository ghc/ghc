%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PatSyntax]{Abstract Haskell syntax---patterns}

\begin{code}
module HsPat (
	Pat(..), InPat, OutPat, 
	
	HsConDetails(..), hsConArgs,

	mkPrefixConPat, mkCharLitPat, mkNilPat,

	isWildPat, 
	patsAreAllCons, isConPat, isSigPat,
	patsAreAllLits,	isLitPat,
	collectPatBinders, collectPatsBinders,
	collectSigTysFromPat, collectSigTysFromPats
    ) where

#include "HsVersions.h"


import {-# SOURCE #-} HsExpr		( HsExpr )

-- friends:
import HsLit		( HsLit(HsCharPrim), HsOverLit )
import HsTypes		( HsType, SyntaxName, PostTcType )
import BasicTypes	( Boxity, tupleParens )
-- others:
import TysWiredIn	( nilDataCon, charDataCon, charTy )
import Var		( TyVar )
import DataCon		( DataCon )
import Outputable	
import Type		( Type )
\end{code}


\begin{code}
type InPat id = Pat id		-- No 'Out' constructors
type OutPat id = Pat id		-- No 'In' constructors

data Pat id
  =	------------ Simple patterns ---------------
    WildPat	PostTcType		-- Wild card
  | VarPat	id			-- Variable
  | LazyPat	(Pat id)		-- Lazy pattern
  | AsPat	id (Pat id)		-- As pattern
  | ParPat      (Pat id)		-- Parenthesised pattern

	------------ Lists, tuples, arrays ---------------
  | ListPat	[Pat id]		-- Syntactic list
		PostTcType		-- The type of the elements
   	    	    
  | TuplePat	[Pat id]		-- Tuple
		Boxity			-- UnitPat is TuplePat []

  | PArrPat	[Pat id]		-- Syntactic parallel array
		PostTcType		-- The type of the elements

	------------ Constructor patterns ---------------
  | ConPatIn	id 
		(HsConDetails id (Pat id))

  | ConPatOut	DataCon 
		(HsConDetails id (Pat id))
		Type    		-- The type of the pattern
		[TyVar]			-- Existentially bound type variables
		[id]			-- Ditto dictionaries

	------------ Literal and n+k patterns ---------------
  | LitPat	    HsLit		-- Used for *non-overloaded* literal patterns:
					-- Int#, Char#, Int, Char, String, etc.

  | NPatIn	    HsOverLit 		-- Always positive
		    (Maybe SyntaxName)	-- Just (Name of 'negate') for negative
					-- patterns, Nothing otherwise

  | NPatOut	    HsLit		-- Used for literal patterns where there's an equality function to call
		    			-- The literal is retained so that the desugarer can readily identify
					-- equations with identical literal-patterns
					-- Always HsInteger, HsRat or HsString.
					-- Always HsInteger, HsRat or HsString.
					-- *Unlike* NPatIn, for negative literals, the
					-- 	literal is acutally negative!
		    Type	 	-- Type of pattern, t
   	    	    (HsExpr id)		-- Of type t -> Bool; detects match

  | NPlusKPatIn	    id			-- n+k pattern
		    HsOverLit		-- It'll always be an HsIntegral
		    SyntaxName		-- Name of '-' (see RnEnv.lookupSyntaxName)

  | NPlusKPatOut    id
		    Integer
   	    	    (HsExpr id)	 	-- Of type t -> Bool; detects match
   	    	    (HsExpr id) 	-- Of type t -> t; subtracts k


	------------ Generics ---------------
  | TypePat	    (HsType id)		-- Type pattern for generic definitions
                                        -- e.g  f{| a+b |} = ...
                                        -- These show up only in class declarations,
                                        -- and should be a top-level pattern

	------------ Pattern type signatures ---------------
  | SigPatIn	    (Pat id)		-- Pattern with a type signature
		    (HsType id)

  | SigPatOut	    (Pat id)		-- Pattern p
		    Type		-- Type, t, of the whole pattern
		    (HsExpr id)		-- Coercion function,
					-- of type t -> typeof(p)

	------------ Dictionary patterns (translation only) ---------------
  | DictPat	    -- Used when destructing Dictionaries with an explicit case
		    [id]			-- superclass dicts
		    [id]			-- methods
\end{code}

HsConDetails is use both for patterns and for data type declarations

\begin{code}
data HsConDetails id arg
  = PrefixCon [arg]			-- C p1 p2 p3
  | RecCon    [(id, arg)]		-- C { x = p1, y = p2 }
  | InfixCon  arg arg			-- p1 `C` p2

hsConArgs :: HsConDetails id arg -> [arg]
hsConArgs (PrefixCon ps)   = ps
hsConArgs (RecCon fs)      = map snd fs
hsConArgs (InfixCon p1 p2) = [p1,p2]
\end{code}


%************************************************************************
%*									*
%* 		Printing patterns
%*									*
%************************************************************************

\begin{code}
instance (OutputableBndr name) => Outputable (Pat name) where
    ppr = pprPat

pprPat :: (OutputableBndr name) => Pat name -> SDoc

pprPat (VarPat var)	  	-- Print with type info if -dppr-debug is on
  = getPprStyle $ \ sty ->
    if debugStyle sty then
	parens (pprBndr LambdaBind var)		-- Could pass the site to pprPat
						-- but is it worth it?
    else
	ppr var

pprPat (WildPat _)	  = char '_'
pprPat (LazyPat pat)      = char '~' <> ppr pat
pprPat (AsPat name pat)   = parens (hcat [ppr name, char '@', ppr pat])
pprPat (ParPat pat)	  = parens (pprPat pat)

pprPat (ListPat pats _)   = brackets (interpp'SP pats)
pprPat (PArrPat pats _)   = pabrackets (interpp'SP pats)
pprPat (TuplePat pats bx) = tupleParens bx (interpp'SP pats)

pprPat (ConPatIn c details) 	   = pprConPat c details
pprPat (ConPatOut c details _ _ _) = pprConPat c details

pprPat (LitPat s)	      = ppr s
pprPat (NPatIn l _)	      = ppr l
pprPat (NPatOut l _ _)        = ppr l
pprPat (NPlusKPatIn n k _)    = hcat [ppr n, char '+', ppr k]
pprPat (NPlusKPatOut n k _ _) = hcat [ppr n, char '+', integer k]

pprPat (TypePat ty) = ptext SLIT("{|") <> ppr ty <> ptext SLIT("|}")

pprPat (SigPatIn pat ty)    = ppr pat <+> dcolon <+> ppr ty
pprPat (SigPatOut pat ty _) = ppr pat <+> dcolon <+> ppr ty

pprPat (DictPat dicts methods)
 = parens (sep [ptext SLIT("{-dict-}"),
		  brackets (interpp'SP dicts),
		  brackets (interpp'SP methods)])



pprConPat con (PrefixCon pats) 	   = ppr con <+> interppSP pats -- inner ParPats supply the necessary parens.
pprConPat con (InfixCon pat1 pat2) = hsep [ppr pat1, ppr con, ppr pat2] -- ParPats put in parens
	-- ToDo: use pprSym to print op (but this involves fiddling various
	-- contexts & I'm lazy...); *PatIns are *rarely* printed anyway... (WDP)
pprConPat con (RecCon rpats)
  = ppr con <+> braces (hsep (punctuate comma (map (pp_rpat) rpats)))
  where
    pp_rpat (v, p) = hsep [ppr v, char '=', ppr p]


-- add parallel array brackets around a document
--
pabrackets   :: SDoc -> SDoc
pabrackets p  = ptext SLIT("[:") <> p <> ptext SLIT(":]")
\end{code}


%************************************************************************
%*									*
%* 		Building patterns
%*									*
%************************************************************************

\begin{code}
mkPrefixConPat :: DataCon -> [OutPat id] -> Type -> OutPat id
-- Make a vanilla Prefix constructor pattern
mkPrefixConPat dc pats ty = ConPatOut dc (PrefixCon pats) ty [] []

mkNilPat :: Type -> OutPat id
mkNilPat ty = mkPrefixConPat nilDataCon [] ty

mkCharLitPat :: Int -> OutPat id
mkCharLitPat c = mkPrefixConPat charDataCon [LitPat (HsCharPrim c)] charTy
\end{code}


%************************************************************************
%*									*
%* Predicates for checking things about pattern-lists in EquationInfo	*
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
isWildPat (WildPat _) = True
isWildPat other	      = False

patsAreAllCons :: [Pat id] -> Bool
patsAreAllCons pat_list = all isConPat pat_list

isConPat (AsPat _ pat)		= isConPat pat
isConPat (ConPatIn _ _)		= True
isConPat (ConPatOut _ _ _ _ _)	= True
isConPat (ListPat _ _)		= True
isConPat (PArrPat _ _)		= True
isConPat (TuplePat _ _)		= True
isConPat (DictPat ds ms)	= (length ds + length ms) > 1
isConPat other			= False

isSigPat (SigPatIn _ _)    = True
isSigPat (SigPatOut _ _ _) = True
isSigPat other		   = False

patsAreAllLits :: [Pat id] -> Bool
patsAreAllLits pat_list = all isLitPat pat_list

isLitPat (AsPat _ pat)	        = isLitPat pat
isLitPat (LitPat _)	        = True
isLitPat (NPatIn _ _)	        = True
isLitPat (NPatOut   _ _ _)      = True
isLitPat (NPlusKPatIn _ _ _)    = True
isLitPat (NPlusKPatOut _ _ _ _) = True
isLitPat other		        = False
\end{code}

%************************************************************************
%*									*
%* 		Gathering stuff out of patterns
%*									*
%************************************************************************

This function @collectPatBinders@ works with the ``collectBinders''
functions for @HsBinds@, etc.  The order in which the binders are
collected is important; see @HsBinds.lhs@.

It collects the bounds *value* variables in renamed patterns; type variables
are *not* collected.

\begin{code}
collectPatBinders :: Pat a -> [a]
collectPatBinders pat = collect pat []

collectPatsBinders :: [Pat a] -> [a]
collectPatsBinders pats = foldr collect [] pats

collect (WildPat _)	      	 bndrs = bndrs
collect (VarPat var)      	 bndrs = var : bndrs
collect (LazyPat pat)     	 bndrs = collect pat bndrs
collect (AsPat a pat)     	 bndrs = a : collect pat bndrs
collect (ParPat  pat)     	 bndrs = collect pat bndrs

collect (ListPat pats _)    	 bndrs = foldr collect bndrs pats
collect (PArrPat pats _)    	 bndrs = foldr collect bndrs pats
collect (TuplePat pats _)  	 bndrs = foldr collect bndrs pats

collect (ConPatIn c ps)   	 bndrs = foldr collect bndrs (hsConArgs ps)
collect (ConPatOut c ps _ _ ds)	 bndrs = ds ++ foldr collect bndrs (hsConArgs ps)

collect (LitPat _)	      	 bndrs = bndrs
collect (NPatIn _ _)		 bndrs = bndrs
collect (NPatOut _ _ _)		 bndrs = bndrs

collect (NPlusKPatIn n _ _)      bndrs = n : bndrs
collect (NPlusKPatOut n _ _ _)   bndrs = n : bndrs

collect (SigPatIn pat _)	 bndrs = collect pat bndrs
collect (SigPatOut pat _ _)	 bndrs = collect pat bndrs
collect (TypePat ty)             bndrs = bndrs
collect (DictPat ids1 ids2)      bndrs = ids1 ++ ids2 ++ bndrs
\end{code}

\begin{code}
collectSigTysFromPats :: [InPat name] -> [HsType name]
collectSigTysFromPats pats = foldr collect_pat [] pats

collectSigTysFromPat :: InPat name -> [HsType name]
collectSigTysFromPat pat = collect_pat pat []

collect_pat (SigPatIn pat ty)  acc = collect_pat pat (ty:acc)
collect_pat (TypePat ty)       acc = ty:acc

collect_pat (LazyPat pat)      acc = collect_pat pat acc
collect_pat (AsPat a pat)      acc = collect_pat pat acc
collect_pat (ParPat  pat)      acc = collect_pat pat acc
collect_pat (ListPat pats _)   acc = foldr collect_pat acc pats
collect_pat (PArrPat pats _)   acc = foldr collect_pat acc pats
collect_pat (TuplePat pats _)  acc = foldr collect_pat acc pats
collect_pat (ConPatIn c ps)    acc = foldr collect_pat acc (hsConArgs ps)
collect_pat other	       acc = acc 	-- Literals, vars, wildcard
\end{code}

