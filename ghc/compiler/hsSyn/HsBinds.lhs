%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[HsBinds]{Abstract syntax: top-level bindings and signatures}

Datatype for: @HsBinds@, @Bind@, @Sig@, @MonoBinds@.

\begin{code}
module HsBinds where

#include "HsVersions.h"

import {-# SOURCE #-} HsExpr    ( pprExpr, HsExpr )
import {-# SOURCE #-} HsMatches ( pprMatches, Match, pprGRHSsAndBinds, GRHSsAndBinds )

-- friends:
import HsPragmas	( GenPragmas, ClassOpPragmas )
import HsTypes		( HsType )
import CoreSyn		( CoreExpr )
import PprCore		()	   -- Instances for Outputable

--others:
import Id		( DictVar, Id, GenId )
import Name		( OccName, NamedThing(..) )
import BasicTypes	( RecFlag(..) )
import Outputable	
import Bag
import SrcLoc		( SrcLoc )
import Type		( GenType )
import TyVar		( GenTyVar )
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
data HsBinds flexi id pat		-- binders and bindees
  = EmptyBinds

  | ThenBinds	(HsBinds flexi id pat)
		(HsBinds flexi id pat)

  | MonoBind 	(MonoBinds flexi id pat)
		[Sig id]		-- Empty on typechecker output
		RecFlag
\end{code}

\begin{code}
nullBinds :: HsBinds flexi id pat -> Bool

nullBinds EmptyBinds		= True
nullBinds (ThenBinds b1 b2)	= nullBinds b1 && nullBinds b2
nullBinds (MonoBind b _ _)	= nullMonoBinds b
\end{code}

\begin{code}
instance (Outputable pat, NamedThing id, Outputable id) =>
		Outputable (HsBinds flexi id pat) where
    ppr binds = ppr_binds binds

ppr_binds EmptyBinds = empty
ppr_binds (ThenBinds binds1 binds2)
     = ($$) (ppr_binds binds1) (ppr_binds binds2)
ppr_binds (MonoBind bind sigs is_rec)
     = vcat [ifNotPprForUser (ptext rec_str),
     	     vcat (map ppr sigs),
	     ppr bind
       ]
     where
       rec_str = case is_rec of
		   Recursive    -> SLIT("{- rec -}")
		   NonRecursive -> SLIT("{- nonrec -}")
\end{code}

%************************************************************************
%*									*
\subsection{Bindings: @MonoBinds@}
%*									*
%************************************************************************

Global bindings (where clauses)

\begin{code}
data MonoBinds flexi id pat
  = EmptyMonoBinds

  | AndMonoBinds    (MonoBinds flexi id pat)
		    (MonoBinds flexi id pat)

  | PatMonoBind     pat
		    (GRHSsAndBinds flexi id pat)
		    SrcLoc

  | FunMonoBind     id
		    Bool			-- True => infix declaration
		    [Match flexi id pat]	-- must have at least one Match
		    SrcLoc

  | VarMonoBind	    id			-- TRANSLATION
		    (HsExpr flexi id pat)

  | CoreMonoBind    id			-- TRANSLATION
		    CoreExpr		-- No zonking; this is a final CoreExpr with Ids and Types!

  | AbsBinds			-- Binds abstraction; TRANSLATION
		[GenTyVar flexi]	  -- Type variables
		[id]			  -- Dicts
		[([GenTyVar flexi], id, id)]  -- (type variables, polymorphic, momonmorphic) triples
		(MonoBinds flexi id pat)      -- The "business end"

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
nullMonoBinds :: MonoBinds flexi id pat -> Bool

nullMonoBinds EmptyMonoBinds	     = True
nullMonoBinds (AndMonoBinds bs1 bs2) = nullMonoBinds bs1 && nullMonoBinds bs2
nullMonoBinds other_monobind	     = False

andMonoBinds :: [MonoBinds flexi id pat] -> MonoBinds flexi id pat
andMonoBinds binds = foldr AndMonoBinds EmptyMonoBinds binds
\end{code}

\begin{code}
instance (NamedThing id, Outputable id, Outputable pat) =>
		Outputable (MonoBinds flexi id pat) where
    ppr mbind = ppr_monobind mbind


ppr_monobind EmptyMonoBinds = empty
ppr_monobind (AndMonoBinds binds1 binds2)
      = ($$) (ppr_monobind binds1) (ppr_monobind binds2)

ppr_monobind (PatMonoBind pat grhss_n_binds locn)
      = sep [ppr pat, nest 4 (pprGRHSsAndBinds False grhss_n_binds)]

ppr_monobind (FunMonoBind fun inf matches locn)
      = pprMatches (False, ppr fun) matches
      -- ToDo: print infix if appropriate

ppr_monobind (VarMonoBind name expr)
      = sep [ppr name <+> equals, nest 4 (pprExpr expr)]

ppr_monobind (CoreMonoBind name expr)
      = sep [ppr name <+> equals, nest 4 (ppr expr)]

ppr_monobind (AbsBinds tyvars dictvars exports val_binds)
     = ($$) (sep [ptext SLIT("AbsBinds"),
		  brackets (interpp'SP tyvars),
		  brackets (interpp'SP dictvars),
		  brackets (interpp'SP exports)])
	       (nest 4 (ppr val_binds))
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

  | MagicUnfoldingSig
		name		-- Associate the "name"d function with
		FAST_STRING	-- the compiler-builtin unfolding (known
		SrcLoc		-- by the String name)
\end{code}

\begin{code}
instance (NamedThing name, Outputable name) => Outputable (Sig name) where
    ppr sig = ppr_sig sig


ppr_sig (Sig var ty _)
      = sep [ppr var <+> ptext SLIT("::"),
	     nest 4 (ppr ty)]

ppr_sig (ClassOpSig var _ ty _)
      = sep [ppr (getOccName var) <+> ptext SLIT("::"),
	     nest 4 (ppr ty)]

ppr_sig (SpecSig var ty using _)
      = sep [ hsep [text "{-# SPECIALIZE", ppr var, ptext SLIT("::")],
	      nest 4 (hsep [ppr ty, pp_using using, text "#-}"])
	]
      where
	pp_using Nothing   = empty
	pp_using (Just me) = hsep [char '=', ppr me]

ppr_sig (InlineSig var _)
        = hsep [text "{-# INLINE", ppr var, text "#-}"]

ppr_sig (MagicUnfoldingSig var str _)
      = hsep [text "{-# MAGIC_UNFOLDING", ppr var, ptext str, text "#-}"]
\end{code}

