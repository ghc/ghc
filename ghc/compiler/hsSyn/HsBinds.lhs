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
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(HsLoop)	( pprMatches, pprGRHSsAndBinds,
			  Match, GRHSsAndBinds,
			  pprExpr, HsExpr )
#endif

import HsPragmas	( GenPragmas, ClassOpPragmas )
import HsTypes		( HsType )
import CoreSyn		( SYN_IE(CoreExpr) )

--others:
import Id		( SYN_IE(DictVar), SYN_IE(Id), GenId )
import Name		( OccName, NamedThing(..) )
import Outputable	( interpp'SP, ifnotPprForUser, pprQuote,
			  Outputable(..){-instance * (,)-}
			)
import PprCore		--( GenCoreExpr {- instance Outputable -} )
import PprType		( GenTyVar {- instance Outputable -} )
import Pretty
import Bag
import SrcLoc		( SrcLoc{-instances-} )
import TyVar		( GenTyVar{-instances-} )
import Unique		( Unique {- instance Eq -} )

#if __GLASGOW_HASKELL__ >= 202
import {-# SOURCE #-} HsExpr    ( pprExpr, HsExpr )
import {-# SOURCE #-} HsMatches ( pprMatches, Match, pprGRHSsAndBinds, GRHSsAndBinds )
#endif

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

  | MonoBind 	(MonoBinds tyvar uvar id pat)
		[Sig id]		-- Empty on typechecker output
		RecFlag

type RecFlag = Bool
recursive    = True
nonRecursive = False
\end{code}

\begin{code}
nullBinds :: HsBinds tyvar uvar id pat -> Bool

nullBinds EmptyBinds		= True
nullBinds (ThenBinds b1 b2)	= nullBinds b1 && nullBinds b2
nullBinds (MonoBind b _ _)	= nullMonoBinds b
\end{code}

\begin{code}
instance (Outputable pat, NamedThing id, Outputable id,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar) =>
		Outputable (HsBinds tyvar uvar id pat) where

    ppr sty binds = pprQuote sty (\ sty -> ppr_binds sty binds)

ppr_binds sty EmptyBinds = empty
ppr_binds sty (ThenBinds binds1 binds2)
     = ($$) (ppr_binds sty binds1) (ppr_binds sty binds2)
ppr_binds sty (MonoBind bind sigs is_rec)
     = vcat [
	ifnotPprForUser sty (ptext rec_str),
	if null sigs
	  then empty
	  else vcat (map (ppr sty) sigs),
	ppr sty bind
       ]
     where
       rec_str | is_rec    = SLIT("{- rec -}")
               | otherwise = SLIT("{- nonrec -}")
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

  | AbsBinds			-- Binds abstraction; TRANSLATION
		[tyvar]			  -- Type variables
		[id]			  -- Dicts
		[([tyvar], id, id)]	  -- (type variables, polymorphic, momonmorphic) triples
		(MonoBinds tyvar uvar id pat)	 -- The "business end"

	-- Creates bindings for *new* (polymorphic, overloaded) locals
	-- in terms of *old* (monomorphic, non-overloaded) ones.
	--
	-- See section 9 of static semantics paper for more details.
	-- (You can get a PhD for explaining the True Meaning
	--  of this last construct.)
\end{code}

What AbsBinds means
~~~~~~~~~~~~~~~~~~~
	 AbsBinds tvs
		  [d1,d2]
		  [(tvs1, f1p, f1m), 
		   (tvs2, f2p, f2m)]
		  BIND
means

	f1p = /\ tvs -> \ [d1,d2] -> letrec DBINDS and BIND 
				      in fm

	gp = ...same again, with gm instead of fm

This is a pretty bad translation, because it duplicates all the bindings.
So the desugarer tries to do a better job:

	fp = /\ [a,b] -> \ [d1,d2] -> case tp [a,b] [d1,d2] of
					(fm,gm) -> fm
	..ditto for gp..

	p = /\ [a,b] -> \ [d1,d2] -> letrec DBINDS and BIND 
				      in (fm,gm)

\begin{code}
nullMonoBinds :: MonoBinds tyvar uvar id pat -> Bool

nullMonoBinds EmptyMonoBinds	     = True
nullMonoBinds (AndMonoBinds bs1 bs2) = nullMonoBinds bs1 && nullMonoBinds bs2
nullMonoBinds other_monobind	     = False

andMonoBinds :: [MonoBinds tyvar uvar id pat] -> MonoBinds tyvar uvar id pat
andMonoBinds binds = foldr AndMonoBinds EmptyMonoBinds binds
\end{code}

\begin{code}
instance (NamedThing id, Outputable id, Outputable pat,
	  Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar) =>
		Outputable (MonoBinds tyvar uvar id pat) where
    ppr sty mbind = pprQuote sty (\ sty -> ppr_monobind sty mbind)


ppr_monobind sty EmptyMonoBinds = empty
ppr_monobind sty (AndMonoBinds binds1 binds2)
      = ($$) (ppr_monobind sty binds1) (ppr_monobind sty binds2)

ppr_monobind sty (PatMonoBind pat grhss_n_binds locn)
      = sep [ppr sty pat, nest 4 (pprGRHSsAndBinds sty False grhss_n_binds)]

ppr_monobind sty (FunMonoBind fun inf matches locn)
      = pprMatches sty (False, ppr sty fun) matches
      -- ToDo: print infix if appropriate

ppr_monobind sty (VarMonoBind name expr)
      = sep [ppr sty name <+> equals, nest 4 (pprExpr sty expr)]

ppr_monobind sty (CoreMonoBind name expr)
      = sep [ppr sty name <+> equals, nest 4 (ppr sty expr)]

ppr_monobind sty (AbsBinds tyvars dictvars exports val_binds)
     = ($$) (sep [ptext SLIT("AbsBinds"),
		  brackets (interpp'SP sty tyvars),
		  brackets (interpp'SP sty dictvars),
		  brackets (interpp'SP sty exports)])
	       (nest 4 (ppr sty val_binds))
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

  | ClassOpSig	name			-- Selector name
		(Maybe name)		-- Default-method name (if any)
		(HsType name)
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
    ppr sty sig = pprQuote sty (\ sty -> ppr_sig sty sig)


ppr_sig sty (Sig var ty _)
      = sep [ppr sty var <+> ptext SLIT("::"),
	     nest 4 (ppr sty ty)]

ppr_sig sty (ClassOpSig var _ ty _)
      = sep [ppr sty (getOccName var) <+> ptext SLIT("::"),
	     nest 4 (ppr sty ty)]

ppr_sig sty (DeforestSig var _)
      = hsep [text "{-# DEFOREST", ppr sty var, text "#-}"]

ppr_sig sty (SpecSig var ty using _)
      = sep [ hsep [text "{-# SPECIALIZE", ppr sty var, ptext SLIT("::")],
	      nest 4 (hsep [ppr sty ty, pp_using using, text "#-}"])
	]
      where
	pp_using Nothing   = empty
	pp_using (Just me) = hsep [char '=', ppr sty me]

ppr_sig sty (InlineSig var _)
        = hsep [text "{-# INLINE", ppr sty var, text "#-}"]

ppr_sig sty (MagicUnfoldingSig var str _)
      = hsep [text "{-# MAGIC_UNFOLDING", ppr sty var, ptext str, text "#-}"]
\end{code}

