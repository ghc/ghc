%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsBinds]{Abstract syntax: top-level bindings and signatures}

Datatype for: @HsBinds@, @Bind@, @Sig@, @MonoBinds@.

\begin{code}
module HsBinds where

#include "HsVersions.h"

import {-# SOURCE #-} HsExpr    ( pprExpr, HsExpr )
import {-# SOURCE #-} HsMatches ( pprMatches, Match, pprGRHSs, GRHSs )

-- friends:
import HsTypes		( HsType )
import HsImpExp		( IE(..), ieName )
import CoreSyn		( CoreExpr )
import PprCore		()	   -- Instances for Outputable

--others:
import Id		( Id )
import NameSet		( NameSet, nameSetToList )
import BasicTypes	( RecFlag(..), Fixity )
import Outputable	
import Bag
import SrcLoc		( SrcLoc )
import Var		( TyVar )
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
data HsBinds id pat		-- binders and bindees
  = EmptyBinds

  | ThenBinds	(HsBinds id pat)
		(HsBinds id pat)

  | MonoBind 	(MonoBinds id pat)
		[Sig id]		-- Empty on typechecker output
		RecFlag
\end{code}

\begin{code}
nullBinds :: HsBinds id pat -> Bool

nullBinds EmptyBinds		= True
nullBinds (ThenBinds b1 b2)	= nullBinds b1 && nullBinds b2
nullBinds (MonoBind b _ _)	= nullMonoBinds b

mkMonoBind :: MonoBinds id pat -> [Sig id] -> RecFlag -> HsBinds id pat
mkMonoBind EmptyMonoBinds _ _ = EmptyBinds
mkMonoBind mbinds sigs is_rec = MonoBind mbinds sigs is_rec
\end{code}

\begin{code}
instance (Outputable pat, Outputable id) =>
		Outputable (HsBinds id pat) where
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
data MonoBinds id pat
  = EmptyMonoBinds

  | AndMonoBinds    (MonoBinds id pat)
		    (MonoBinds id pat)

  | PatMonoBind     pat
		    (GRHSs id pat)
		    SrcLoc

  | FunMonoBind     id
		    Bool		-- True => infix declaration
		    [Match id pat]
		    SrcLoc

  | VarMonoBind	    id			-- TRANSLATION
		    (HsExpr id pat)

  | CoreMonoBind    id			-- TRANSLATION
		    CoreExpr		-- No zonking; this is a final CoreExpr with Ids and Types!

  | AbsBinds				-- Binds abstraction; TRANSLATION
		[TyVar]	  		-- Type variables
		[id]			-- Dicts
		[([TyVar], id, id)]	-- (type variables, polymorphic, momonmorphic) triples
		NameSet			-- Set of *polymorphic* variables that have an INLINE pragma
		(MonoBinds id pat)      -- The "business end"

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
-- We keep the invariant that a MonoBinds is only empty 
-- if it is exactly EmptyMonoBinds

nullMonoBinds :: MonoBinds id pat -> Bool
nullMonoBinds EmptyMonoBinds	     = True
nullMonoBinds other_monobind	     = False

andMonoBinds :: MonoBinds id pat -> MonoBinds id pat -> MonoBinds id pat
andMonoBinds EmptyMonoBinds mb = mb
andMonoBinds mb EmptyMonoBinds = mb
andMonoBinds mb1 mb2 = AndMonoBinds mb1 mb2

andMonoBindList :: [MonoBinds id pat] -> MonoBinds id pat
andMonoBindList binds
  = loop1 binds
  where
    loop1 [] = EmptyMonoBinds
    loop1 (EmptyMonoBinds : binds) = loop1 binds
    loop1 (b:bs) = loop2 b bs

	-- acc is non-empty
    loop2 acc [] = acc
    loop2 acc (EmptyMonoBinds : bs) = loop2 acc bs
    loop2 acc (b:bs) = loop2 (acc `AndMonoBinds` b) bs
\end{code}

\begin{code}
instance (Outputable id, Outputable pat) =>
		Outputable (MonoBinds id pat) where
    ppr mbind = ppr_monobind mbind


ppr_monobind :: (Outputable id, Outputable pat) => MonoBinds id pat -> SDoc
ppr_monobind EmptyMonoBinds = empty
ppr_monobind (AndMonoBinds binds1 binds2)
      = ppr_monobind binds1 $$ ppr_monobind binds2

ppr_monobind (PatMonoBind pat grhss locn)
      = sep [ppr pat, nest 4 (pprGRHSs False grhss)]

ppr_monobind (FunMonoBind fun inf matches locn)
      = pprMatches (False, ppr fun) matches
      -- ToDo: print infix if appropriate

ppr_monobind (VarMonoBind name expr)
      = sep [ppr name <+> equals, nest 4 (pprExpr expr)]

ppr_monobind (CoreMonoBind name expr)
      = sep [ppr name <+> equals, nest 4 (ppr expr)]

ppr_monobind (AbsBinds tyvars dictvars exports inlines val_binds)
     = sep [ptext SLIT("AbsBinds"),
	    brackets (interpp'SP tyvars),
	    brackets (interpp'SP dictvars),
	    brackets (sep (punctuate comma (map ppr exports))),
	    brackets (interpp'SP (nameSetToList inlines))]
       $$
       nest 4 (ppr val_binds)
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

  | ClassOpSig	name		-- Selector name
		name		-- Default-method name (if any)
		Bool		-- True <=> there is an explicit, programmer-supplied
				-- default declaration in the class decl
		(HsType name)
		SrcLoc

  | SpecSig 	name		-- specialise a function or datatype ...
		(HsType name)	-- ... to these types
		SrcLoc

  | InlineSig	name		-- INLINE f
	 	(Maybe Int)	-- phase
		SrcLoc

  | NoInlineSig	name		-- NOINLINE f
	 	(Maybe Int)	-- phase
		SrcLoc

  | SpecInstSig (HsType name)	-- (Class tys); should be a specialisation of the 
				-- current instance decl
		SrcLoc

  | FixSig	(FixitySig name)	-- Fixity declaration

  | DeprecSig	(Deprecation name)	-- DEPRECATED
		SrcLoc


data FixitySig name  = FixitySig name Fixity SrcLoc

-- We use exported entities for things to deprecate. Cunning trick (hack?):
-- `IEModuleContents undefined' is used for module deprecation.
data Deprecation name = Deprecation (IE name) DeprecTxt

type DeprecTxt = FAST_STRING	-- reason/explanation for deprecation
\end{code}

\begin{code}
sigsForMe :: (name -> Bool) -> [Sig name] -> [Sig name]
sigsForMe f sigs
  = filter sig_for_me sigs
  where
    sig_for_me (Sig         n _ _)                         = f n
    sig_for_me (ClassOpSig  n _ _ _ _)                     = f n
    sig_for_me (SpecSig     n _ _)                         = f n
    sig_for_me (InlineSig   n _   _)                       = f n
    sig_for_me (NoInlineSig n _   _)                       = f n
    sig_for_me (SpecInstSig _ _)                           = False
    sig_for_me (FixSig (FixitySig n _ _))                  = f n
    sig_for_me
	(DeprecSig (Deprecation (IEModuleContents _) _) _) = False
    sig_for_me
	(DeprecSig (Deprecation d                    _) _) = f (ieName d)

isFixitySig :: Sig name -> Bool
isFixitySig (FixSig _) = True
isFixitySig _	       = False

isClassOpSig :: Sig name -> Bool
isClassOpSig (ClassOpSig _ _ _ _ _) = True
isClassOpSig _			    = False

isPragSig :: Sig name -> Bool
	-- Identifies pragmas 
isPragSig (SpecSig _ _ _)     = True
isPragSig (InlineSig   _ _ _) = True
isPragSig (NoInlineSig _ _ _) = True
isPragSig (SpecInstSig _ _)   = True
isPragSig (DeprecSig _ _)     = True
isPragSig other		      = False
\end{code}

\begin{code}
instance (Outputable name) => Outputable (Sig name) where
    ppr sig = ppr_sig sig

ppr_sig :: Outputable name => Sig name -> SDoc
ppr_sig (Sig var ty _)
      = sep [ppr var <+> dcolon, nest 4 (ppr ty)]

ppr_sig (ClassOpSig var _ _ ty _)
      = sep [ppr var <+> dcolon, nest 4 (ppr ty)]

ppr_sig (SpecSig var ty _)
      = sep [ hsep [text "{-# SPECIALIZE", ppr var, dcolon],
	      nest 4 (ppr ty <+> text "#-}")
	]

ppr_sig (InlineSig var phase _)
      = hsep [text "{-# INLINE", ppr_phase phase, ppr var, text "#-}"]

ppr_sig (NoInlineSig var phase _)
      = hsep [text "{-# NOINLINE", ppr_phase phase, ppr var, text "#-}"]

ppr_sig (SpecInstSig ty _)
      = hsep [text "{-# SPECIALIZE instance", ppr ty, text "#-}"]

ppr_sig (FixSig fix_sig) = ppr fix_sig

ppr_sig (DeprecSig deprec _) = ppr deprec

instance Outputable name => Outputable (FixitySig name) where
  ppr (FixitySig name fixity loc) = sep [ppr fixity, ppr name]

instance Outputable name => Outputable (Deprecation name) where
   ppr (Deprecation (IEModuleContents _) txt)
      = hsep [text "{-# DEPRECATED",            doubleQuotes (ppr txt), text "#-}"]
   ppr (Deprecation thing txt)
      = hsep [text "{-# DEPRECATED", ppr thing, doubleQuotes (ppr txt), text "#-}"]

ppr_phase :: Maybe Int -> SDoc
ppr_phase Nothing  = empty
ppr_phase (Just n) = int n
\end{code}

