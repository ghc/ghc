%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsBinds]{Abstract syntax: top-level bindings and signatures}

Datatype for: @HsBinds@, @Bind@, @Sig@, @MonoBinds@.

\begin{code}
#include "HsVersions.h"

module HsBinds where

IMP_Ubiq()

-- friends:
IMPORT_DELOOPER(HsLoop)
import HsMatches	( pprMatches, pprGRHSsAndBinds,
			  Match, GRHSsAndBinds )
import HsPat		( collectPatBinders, InPat )
import HsPragmas	( GenPragmas, ClassOpPragmas )
import HsTypes		( HsType )
import CoreSyn		( SYN_IE(CoreExpr) )

--others:
import Id		( SYN_IE(DictVar), SYN_IE(Id), GenId )
import Name		( pprNonSym, getOccName, OccName )
import Outputable	( interpp'SP, ifnotPprForUser,
			  Outputable(..){-instance * (,)-}
			)
import PprCore		( GenCoreExpr {- instance Outputable -} )
import PprType		( GenTyVar {- instance Outputable -} )
import Pretty
import Bag
import SrcLoc		( SrcLoc{-instances-} )
import TyVar		( GenTyVar{-instances-} )
import Unique		( Unique {- instance Eq -} )
\end{code}

%************************************************************************
%*									*
\subsection{Bindings: @HsBinds@}
%*									*
%************************************************************************

The following syntax may produce new syntax which is not part of the input,
and which is instead a translation of the input to the typechecker.
Syntax translations are marked TRANSLATION in comments. New empty
productions are useful in development but may not appear in the final
grammar.

Collections of bindings, created by dependency analysis and translation:

\begin{code}
data HsBinds tyvar uvar id pat		-- binders and bindees
  = EmptyBinds

  | ThenBinds	(HsBinds tyvar uvar id pat)
		(HsBinds tyvar uvar id pat)

  | SingleBind	(Bind  tyvar uvar id pat)

  | BindWith		-- Bind with a type signature.
			-- These appear only on typechecker input
			-- (HsType [in Sigs] can't appear on output)
		(Bind tyvar uvar id pat)
		[Sig id]

  | AbsBinds			-- Binds abstraction; TRANSLATION
		[tyvar]
		[id]		-- Dicts
		[(id, id)]	-- (old, new) pairs
		[(id, HsExpr tyvar uvar id pat)]	-- local dictionaries
		(Bind tyvar uvar id pat)		-- "the business end"

	-- Creates bindings for *new* (polymorphic, overloaded) locals
	-- in terms of *old* (monomorphic, non-overloaded) ones.
	--
	-- See section 9 of static semantics paper for more details.
	-- (You can get a PhD for explaining the True Meaning
	--  of this last construct.)
\end{code}

\begin{code}
nullBinds :: HsBinds tyvar uvar id pat -> Bool

nullBinds EmptyBinds		= True
nullBinds (ThenBinds b1 b2)	= nullBinds b1 && nullBinds b2
nullBinds (SingleBind b)	= nullBind b
nullBinds (BindWith b _)	= nullBind b
nullBinds (AbsBinds _ _ _ ds b)	= null ds && nullBind b
\end{code}

\begin{code}
instance (Outputable pat, NamedThing id, Outputable id,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar) =>
		Outputable (HsBinds tyvar uvar id pat) where

    ppr sty EmptyBinds = ppNil
    ppr sty (ThenBinds binds1 binds2)
     = ppAbove (ppr sty binds1) (ppr sty binds2)
    ppr sty (SingleBind bind) = ppr sty bind
    ppr sty (BindWith bind sigs)
     = ppAbove (if null sigs 
		then ppNil
		else ppAboves (map (ppr sty) sigs))
	       (ppr sty bind)
    ppr sty (AbsBinds tyvars dictvars local_pairs dict_binds val_binds)
     = ppAbove (ppSep [ppPStr SLIT("AbsBinds"),
		      ppBesides[ppLbrack, interpp'SP sty tyvars, ppRbrack],
		      ppBesides[ppLbrack, interpp'SP sty dictvars, ppRbrack],
		      ppBesides[ppLbrack, interpp'SP sty local_pairs, ppRbrack]])
	    (ppNest 4 (ppAbove (ppAboves (map (ppr sty) dict_binds)) (ppr sty val_binds)))
\end{code}

%************************************************************************
%*									*
\subsection{@Sig@: type signatures and value-modifying user pragmas}
%*									*
%************************************************************************

It is convenient to lump ``value-modifying'' user-pragmas (e.g.,
``specialise this function to these four types...'') in with type
signatures.  Then all the machinery to move them into place, etc.,
serves for both.

\begin{code}
data Sig name
  = Sig		name		-- a bog-std type signature
		(HsType name)
		SrcLoc

  | ClassOpSig	name		-- class-op sigs have different pragmas
		(HsType name)
		(ClassOpPragmas name)	-- only interface ones have pragmas
		SrcLoc

  | SpecSig 	name		-- specialise a function or datatype ...
		(HsType name) -- ... to these types
		(Maybe name)	-- ... maybe using this as the code for it
		SrcLoc

  | InlineSig	name		  -- INLINE f
		SrcLoc

  | DeforestSig name            -- Deforest using this function definition
	      	SrcLoc

  | MagicUnfoldingSig
		name		-- Associate the "name"d function with
		FAST_STRING	-- the compiler-builtin unfolding (known
		SrcLoc		-- by the String name)
\end{code}

\begin{code}
instance (NamedThing name, Outputable name) => Outputable (Sig name) where
    ppr sty (Sig var ty _)
      = ppHang (ppCat [ppr sty var, ppPStr SLIT("::")])
	     4 (ppr sty ty)

    ppr sty (ClassOpSig var ty pragmas _)
      = ppHang (ppCat [ppr sty (getOccName var), ppPStr SLIT("::")])
	     4 (ppHang (ppr sty ty)
		     4 (ifnotPprForUser sty (ppr sty pragmas)))

    ppr sty (DeforestSig var _)
      = ppHang (ppCat [ppStr "{-# DEFOREST", pprNonSym sty var])
		   4 (ppStr "#-}")

    ppr sty (SpecSig var ty using _)
      = ppHang (ppCat [ppPStr SLIT("{-# SPECIALIZE"), pprNonSym sty var, ppPStr SLIT("::")])
	     4 (ppCat [ppr sty ty, pp_using using, ppPStr SLIT("#-}")])
      where
	pp_using Nothing   = ppNil
	pp_using (Just me) = ppCat [ppChar '=', ppr sty me]

    ppr sty (InlineSig var _)
      = ppCat [ppPStr SLIT("{-# INLINE"), pprNonSym sty var, ppPStr SLIT("#-}")]

    ppr sty (MagicUnfoldingSig var str _)
      = ppCat [ppPStr SLIT("{-# MAGIC_UNFOLDING"), pprNonSym sty var, ppPStr str, ppPStr SLIT("#-}")]
\end{code}

%************************************************************************
%*									*
\subsection{Binding: @Bind@}
%*									*
%************************************************************************

\begin{code}
data Bind tyvar uvar id pat		-- binders and bindees
  = EmptyBind	-- because it's convenient when parsing signatures
  | NonRecBind	(MonoBinds tyvar uvar id pat)
  | RecBind	(MonoBinds tyvar uvar id pat)
\end{code}

\begin{code}
nullBind :: Bind tyvar uvar id pat -> Bool

nullBind EmptyBind	 = True
nullBind (NonRecBind bs) = nullMonoBinds bs
nullBind (RecBind bs)	 = nullMonoBinds bs
\end{code}

\begin{code}
bindIsRecursive :: Bind tyvar uvar id pat -> Bool

bindIsRecursive EmptyBind	= False
bindIsRecursive (NonRecBind _)	= False
bindIsRecursive (RecBind _)	= True
\end{code}

\begin{code}
instance (NamedThing id, Outputable id, Outputable pat,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar) =>
		Outputable (Bind tyvar uvar id pat) where
    ppr sty EmptyBind = ppNil
    ppr sty (NonRecBind binds)
     = ppAbove (ifnotPprForUser sty (ppStr "{- nonrec -}"))
	       (ppr sty binds)
    ppr sty (RecBind binds)
     = ppAbove (ifnotPprForUser sty (ppStr "{- rec -}"))
	       (ppr sty binds)
\end{code}

%************************************************************************
%*									*
\subsection{Bindings: @MonoBinds@}
%*									*
%************************************************************************

Global bindings (where clauses)

\begin{code}
data MonoBinds tyvar uvar id pat
  = EmptyMonoBinds
  | AndMonoBinds    (MonoBinds tyvar uvar id pat)
		    (MonoBinds tyvar uvar id pat)
  | PatMonoBind     pat
		    (GRHSsAndBinds tyvar uvar id pat)
		    SrcLoc
  | FunMonoBind     id
		    Bool			-- True => infix declaration
		    [Match tyvar uvar id pat]	-- must have at least one Match
		    SrcLoc

  | VarMonoBind	    id			-- TRANSLATION
		    (HsExpr tyvar uvar id pat)

  | CoreMonoBind    id			-- TRANSLATION
		    CoreExpr		-- No zonking; this is a final CoreExpr with Ids and Types!
\end{code}

\begin{code}
nullMonoBinds :: MonoBinds tyvar uvar id pat -> Bool

nullMonoBinds EmptyMonoBinds	     = True
nullMonoBinds (AndMonoBinds bs1 bs2) = nullMonoBinds bs1 && nullMonoBinds bs2
nullMonoBinds other_monobind	     = False
\end{code}

\begin{code}
instance (NamedThing id, Outputable id, Outputable pat,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar) =>
		Outputable (MonoBinds tyvar uvar id pat) where
    ppr sty EmptyMonoBinds = ppNil
    ppr sty (AndMonoBinds binds1 binds2)
      = ppAbove (ppr sty binds1) (ppr sty binds2)

    ppr sty (PatMonoBind pat grhss_n_binds locn)
      = ppHang (ppr sty pat) 4 (pprGRHSsAndBinds sty False grhss_n_binds)

    ppr sty (FunMonoBind fun inf matches locn)
      = pprMatches sty (False, ppr sty fun) matches
      -- ToDo: print infix if appropriate

    ppr sty (VarMonoBind name expr)
      = ppHang (ppCat [ppr sty name, ppEquals]) 4 (ppr sty expr)

    ppr sty (CoreMonoBind name expr)
      = ppHang (ppCat [ppr sty name, ppEquals]) 4 (ppr sty expr)
\end{code}

%************************************************************************
%*									*
\subsection{Collecting binders from @HsBinds@}
%*									*
%************************************************************************

Get all the binders in some @MonoBinds@, IN THE ORDER OF
APPEARANCE; e.g., in:
\begin{verbatim}
...
where
  (x, y) = ...
  f i j  = ...
  [a, b] = ...
\end{verbatim}
it should return @[x, y, f, a, b]@ (remember, order important).

\begin{code}
collectTopBinders :: HsBinds tyvar uvar name (InPat name) -> Bag (name,SrcLoc)
collectTopBinders EmptyBinds     = emptyBag
collectTopBinders (SingleBind b) = collectBinders b
collectTopBinders (BindWith b _) = collectBinders b
collectTopBinders (ThenBinds b1 b2)
 = collectTopBinders b1 `unionBags` collectTopBinders b2

collectBinders :: Bind tyvar uvar name (InPat name) -> Bag (name,SrcLoc)
collectBinders EmptyBind 	      = emptyBag
collectBinders (NonRecBind monobinds) = collectMonoBinders monobinds
collectBinders (RecBind monobinds)    = collectMonoBinders monobinds

collectMonoBinders :: MonoBinds tyvar uvar name (InPat name) -> Bag (name,SrcLoc)
collectMonoBinders EmptyMonoBinds		       = emptyBag
collectMonoBinders (PatMonoBind pat grhss_w_binds loc) = listToBag (map (\v->(v,loc)) (collectPatBinders pat))
collectMonoBinders (FunMonoBind f _ matches loc)       = unitBag (f,loc)
collectMonoBinders (VarMonoBind v expr) 	       = error "collectMonoBinders"
collectMonoBinders (CoreMonoBind v expr) 	       = error "collectMonoBinders"
collectMonoBinders (AndMonoBinds bs1 bs2)
 = collectMonoBinders bs1 `unionBags` collectMonoBinders bs2
\end{code}
