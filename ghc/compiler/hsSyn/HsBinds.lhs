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
import HsTypes		( PolyType )

--others:
import Id		( DictVar(..), Id(..), GenId )
import Name		( pprNonSym )
import Outputable	( interpp'SP, ifnotPprForUser,
			  Outputable(..){-instance * (,)-}
			)
import Pretty
import SrcLoc		( SrcLoc{-instances-} )
--import TyVar		( GenTyVar{-instances-} )
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
			-- (PolyType [in Sigs] can't appear on output)
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
		(PolyType name)
		(GenPragmas name) -- only interface ones have pragmas
		SrcLoc

  | ClassOpSig	name		-- class-op sigs have different pragmas
		(PolyType name)
		(ClassOpPragmas name)	-- only interface ones have pragmas
		SrcLoc

  | SpecSig 	name		-- specialise a function or datatype ...
		(PolyType name) -- ... to these types
		(Maybe name)	-- ... maybe using this as the code for it
		SrcLoc

  | InlineSig	name		  -- INLINE f
		SrcLoc

  -- ToDo: strictly speaking, could omit based on -DOMIT_DEFORESTER
  | DeforestSig name            -- Deforest using this function definition
	      	SrcLoc

  | MagicUnfoldingSig
		name		-- Associate the "name"d function with
		FAST_STRING	-- the compiler-builtin unfolding (known
		SrcLoc		-- by the String name)
\end{code}

\begin{code}
instance (NamedThing name, Outputable name) => Outputable (Sig name) where
    ppr sty (Sig var ty pragmas _)
      = ppHang (ppCat [pprNonSym sty var, ppPStr SLIT("::")])
	     4 (ppHang (ppr sty ty)
		     4 (ifnotPprForUser sty (ppr sty pragmas)))

    ppr sty (ClassOpSig var ty pragmas _)
      = ppHang (ppCat [pprNonSym sty var, ppPStr SLIT("::")])
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
      = pprMatches sty (False, pprNonSym sty fun) matches
      -- ToDo: print infix if appropriate

    ppr sty (VarMonoBind name expr)
      = ppHang (ppCat [pprNonSym sty name, ppEquals]) 4 (ppr sty expr)
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
collectTopLevelBinders :: HsBinds tyvar uvar name (InPat name) -> [name]
collectTopLevelBinders EmptyBinds     = []
collectTopLevelBinders (SingleBind b) = collectBinders b
collectTopLevelBinders (BindWith b _) = collectBinders b
collectTopLevelBinders (ThenBinds b1 b2)
 = collectTopLevelBinders b1 ++ collectTopLevelBinders b2

collectBinders :: Bind tyvar uvar name (InPat name) -> [name]
collectBinders EmptyBind 	      = []
collectBinders (NonRecBind monobinds) = collectMonoBinders monobinds
collectBinders (RecBind monobinds)    = collectMonoBinders monobinds

collectMonoBinders :: MonoBinds tyvar uvar name (InPat name) -> [name]
collectMonoBinders EmptyMonoBinds		     = []
collectMonoBinders (PatMonoBind pat grhss_w_binds _) = collectPatBinders pat
collectMonoBinders (FunMonoBind f _ matches _)	     = [f]
collectMonoBinders (VarMonoBind v expr) 	     = error "collectMonoBinders"
collectMonoBinders (AndMonoBinds bs1 bs2)
 = collectMonoBinders bs1 ++ collectMonoBinders bs2

-- We'd like the binders -- and where they came from --
-- so we can make new ones with equally-useful origin info.

collectMonoBindersAndLocs
	:: MonoBinds tyvar uvar name (InPat name) -> [(name, SrcLoc)]

collectMonoBindersAndLocs EmptyMonoBinds = []

collectMonoBindersAndLocs (AndMonoBinds bs1 bs2)
  = collectMonoBindersAndLocs bs1 ++ collectMonoBindersAndLocs bs2

collectMonoBindersAndLocs (PatMonoBind pat grhss_w_binds locn)
  = collectPatBinders pat `zip` repeat locn

collectMonoBindersAndLocs (FunMonoBind f _ matches locn) = [(f, locn)]

#ifdef DEBUG
collectMonoBindersAndLocs (VarMonoBind v expr)
  = trace "collectMonoBindersAndLocs:VarMonoBind" []
	-- ToDo: this is dubious, i.e., wrong, but harmless?
#endif
\end{code}
