%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PatSyntax]{Abstract Haskell syntax---patterns}

\begin{code}
module HsPat (
	Pat(..), InPat, OutPat, LPat,
	
	HsConDetails(..), hsConArgs,

	mkPrefixConPat, mkCharLitPat, mkNilPat, 

	isWildPat, 
	patsAreAllCons, isConPat, isSigPat,
	patsAreAllLits,	isLitPat
    ) where

#include "HsVersions.h"


import {-# SOURCE #-} HsExpr		( HsExpr )

-- friends:
import HsBinds		( DictBinds, emptyLHsBinds, pprLHsBinds )
import HsLit		( HsLit(HsCharPrim), HsOverLit )
import HsTypes		( LHsType, SyntaxName, PostTcType )
import BasicTypes	( Boxity, tupleParens )
-- others:
import PprCore		( {- instance OutputableBndr TyVar -} )
import TysWiredIn	( nilDataCon, charDataCon, charTy )
import Var		( TyVar )
import DataCon		( DataCon )
import Outputable	
import Type		( Type )
import SrcLoc		( Located(..), unLoc, noLoc )
\end{code}


\begin{code}
type InPat id  = LPat id	-- No 'Out' constructors
type OutPat id = LPat id	-- No 'In' constructors

type LPat id = Located (Pat id)

data Pat id
  =	------------ Simple patterns ---------------
    WildPat	PostTcType		-- Wild card
  | VarPat	id			-- Variable
  | VarPatOut	id (DictBinds id)	-- Used only for overloaded Ids; the 
					-- bindings give its overloaded instances
  | LazyPat	(LPat id)		-- Lazy pattern
  | AsPat	(Located id) (LPat id)  -- As pattern
  | ParPat      (LPat id)		-- Parenthesised pattern

	------------ Lists, tuples, arrays ---------------
  | ListPat	[LPat id]		-- Syntactic list
		PostTcType		-- The type of the elements
   	    	    
  | TuplePat	[LPat id]		-- Tuple
		Boxity			-- UnitPat is TuplePat []

  | PArrPat	[LPat id]		-- Syntactic parallel array
		PostTcType		-- The type of the elements

	------------ Constructor patterns ---------------
  | ConPatIn	(Located id)
		(HsConDetails id (LPat id))

  | ConPatOut	(Located DataCon)
		[TyVar]			-- Existentially bound type variables
		[id]			-- Ditto dictionaries
		(DictBinds id)		-- Bindings involving those dictionaries
		(HsConDetails id (LPat id))
		Type    		-- The type of the pattern

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
					--  *Unlike* NPatIn, for negative literals, the
					-- 	literal is acutally negative!
		    Type	 	-- Type of pattern, t
   	    	    (HsExpr id)		-- Of type t -> Bool; detects match

  | NPlusKPatIn	    (Located id)	-- n+k pattern
		    HsOverLit		-- It'll always be an HsIntegral
		    SyntaxName		-- Name of '-' (see RnEnv.lookupSyntaxName)

  | NPlusKPatOut    (Located id)
		    Integer
   	    	    (HsExpr id)	 	-- Of type t -> Bool; detects match
   	    	    (HsExpr id) 	-- Of type t -> t; subtracts k


	------------ Generics ---------------
  | TypePat	    (LHsType id)	-- Type pattern for generic definitions
                                        -- e.g  f{| a+b |} = ...
                                        -- These show up only in class declarations,
                                        -- and should be a top-level pattern

	------------ Pattern type signatures ---------------
  | SigPatIn	    (LPat id)		-- Pattern with a type signature
		    (LHsType id)

  | SigPatOut	    (LPat id)		-- Pattern with a type signature
		    Type

	------------ Dictionary patterns (translation only) ---------------
  | DictPat	    -- Used when destructing Dictionaries with an explicit case
		    [id]			-- superclass dicts
		    [id]			-- methods
\end{code}

HsConDetails is use both for patterns and for data type declarations

\begin{code}
data HsConDetails id arg
  = PrefixCon [arg]			-- C p1 p2 p3
  | RecCon    [(Located id, arg)]	-- C { x = p1, y = p2 }
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

pprPatBndr :: OutputableBndr name => name -> SDoc
pprPatBndr var		  	-- Print with type info if -dppr-debug is on
  = getPprStyle $ \ sty ->
    if debugStyle sty then
	parens (pprBndr LambdaBind var)		-- Could pass the site to pprPat
						-- but is it worth it?
    else
	ppr var

pprPat :: (OutputableBndr name) => Pat name -> SDoc

pprPat (VarPat var)  	  = pprPatBndr var
pprPat (VarPatOut var bs) = parens (pprPatBndr var <+> braces (ppr bs))
pprPat (WildPat _)	  = char '_'
pprPat (LazyPat pat)      = char '~' <> ppr pat
pprPat (AsPat name pat)   = parens (hcat [ppr name, char '@', ppr pat])
pprPat (ParPat pat)	  = parens (ppr pat)

pprPat (ListPat pats _)   = brackets (interpp'SP pats)
pprPat (PArrPat pats _)   = pabrackets (interpp'SP pats)
pprPat (TuplePat pats bx) = tupleParens bx (interpp'SP pats)

pprPat (ConPatIn con details) = pprUserCon con details
pprPat (ConPatOut con tvs dicts binds details _) 
  = getPprStyle $ \ sty ->	-- Tiresome; in TcBinds.tcRhs we print out a 
    if debugStyle sty then 	-- typechecked Pat in an error message, 
				-- and we want to make sure it prints nicely
	ppr con <+> sep [ hsep (map pprPatBndr tvs) <+> hsep (map pprPatBndr dicts),
		   	  pprLHsBinds binds, pprConArgs details]
    else pprUserCon con details

pprPat (LitPat s)	      = ppr s
pprPat (NPatIn l _)	      = ppr l
pprPat (NPatOut l _ _)        = ppr l
pprPat (NPlusKPatIn n k _)    = hcat [ppr n, char '+', ppr k]
pprPat (NPlusKPatOut n k _ _) = hcat [ppr n, char '+', integer k]
pprPat (TypePat ty)	      = ptext SLIT("{|") <> ppr ty <> ptext SLIT("|}")
pprPat (SigPatIn pat ty)      = ppr pat <+> dcolon <+> ppr ty
pprPat (SigPatOut pat ty)     = ppr pat <+> dcolon <+> ppr ty
pprPat (DictPat ds ms)	      = parens (sep [ptext SLIT("{-dict-}"),
					     brackets (interpp'SP ds),
					     brackets (interpp'SP ms)])

pprUserCon c (InfixCon p1 p2) = ppr p1 <+> ppr c <+> ppr p2
pprUserCon c details          = ppr c <+> pprConArgs details

pprConArgs (PrefixCon pats) = interppSP pats
pprConArgs (InfixCon p1 p2) = interppSP [p1,p2]
pprConArgs (RecCon rpats)   = braces (hsep (punctuate comma (map (pp_rpat) rpats)))
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
mkPrefixConPat dc pats ty = noLoc $ ConPatOut (noLoc dc) [] [] emptyLHsBinds (PrefixCon pats) ty

mkNilPat :: Type -> OutPat id
mkNilPat ty = mkPrefixConPat nilDataCon [] ty

mkCharLitPat :: Char -> OutPat id
mkCharLitPat c = mkPrefixConPat charDataCon [noLoc $ LitPat (HsCharPrim c)] charTy
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

isConPat (AsPat _ pat)		 = isConPat (unLoc pat)
isConPat (ConPatIn _ _)		 = True
isConPat (ConPatOut _ _ _ _ _ _) = True
isConPat (ListPat _ _)		 = True
isConPat (PArrPat _ _)		 = True
isConPat (TuplePat _ _)		 = True
isConPat (DictPat ds ms)	 = (length ds + length ms) > 1
isConPat other			 = False

isSigPat (SigPatIn _ _)  = True
isSigPat (SigPatOut _ _) = True
isSigPat other		 = False

patsAreAllLits :: [Pat id] -> Bool
patsAreAllLits pat_list = all isLitPat pat_list

isLitPat (AsPat _ pat)	        = isLitPat (unLoc pat)
isLitPat (LitPat _)	        = True
isLitPat (NPatIn _ _)	        = True
isLitPat (NPatOut   _ _ _)      = True
isLitPat (NPlusKPatIn _ _ _)    = True
isLitPat (NPlusKPatOut _ _ _ _) = True
isLitPat other		        = False
\end{code}

