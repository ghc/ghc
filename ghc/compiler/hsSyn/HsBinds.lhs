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
import Name		( Name, isUnboundName )
import NameSet		( NameSet, elemNameSet, nameSetToList )
import BasicTypes	( RecFlag(..), Fixity )
import Outputable	
import Bag
import SrcLoc		( SrcLoc )
import Var		( TyVar )
import Util		( thenCmp )
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

  | FunMonoBind     id		-- Used for both functions 	f x = e
				-- and variables		f = \x -> e
				-- Reason: the Match stuff lets us have an optional
				--	   result type sig	f :: a->a = ...mentions a...
		    Bool		-- True => infix declaration
		    [Match id pat]
		    SrcLoc

  | PatMonoBind     pat		-- The pattern is never a simple variable;
				-- That case is done by FunMonoBind
		    (GRHSs id pat)
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


data FixitySig name = FixitySig name Fixity SrcLoc 

instance Eq name => Eq (FixitySig name) where
   (FixitySig n1 f1 _) == (FixitySig n2 f2 _) = n1==n2 && f1==f2
\end{code}

\begin{code}
okBindSig :: NameSet -> Sig Name -> Bool
okBindSig ns (ClassOpSig _ _ _ _ _)				= False
okBindSig ns sig = sigForThisGroup ns sig

okClsDclSig :: NameSet -> Sig Name -> Bool
okClsDclSig ns (Sig _ _ _)					  = False
okClsDclSig ns sig = sigForThisGroup ns sig

okInstDclSig :: NameSet -> Sig Name -> Bool
okInstDclSig ns (Sig _ _ _)					   = False
okInstDclSig ns (FixSig _)					   = False
okInstDclSig ns (SpecInstSig _ _)				   = True
okInstDclSig ns sig = sigForThisGroup ns sig

sigForThisGroup ns sig 
  = case sigName sig of
	Nothing 		 -> False
	Just n | isUnboundName n -> True	-- Don't complain about an unbound name again
	       | otherwise 	 -> n `elemNameSet` ns

sigName :: Sig name -> Maybe name
sigName (Sig         n _ _)             = Just n
sigName (ClassOpSig  n _ _ _ _)         = Just n
sigName (SpecSig     n _ _)             = Just n
sigName (InlineSig   n _   _)           = Just n
sigName (NoInlineSig n _   _)           = Just n
sigName (FixSig (FixitySig n _ _))      = Just n
sigName other				= Nothing

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
isPragSig other		      = False
\end{code}

\begin{code}
hsSigDoc (Sig        _ _ loc) 	      = (SLIT("type signature"),loc)
hsSigDoc (ClassOpSig _ _ _ _ loc)     = (SLIT("class-method type signature"), loc)
hsSigDoc (SpecSig    _ _ loc) 	      = (SLIT("SPECIALISE pragma"),loc)
hsSigDoc (InlineSig  _ _    loc)      = (SLIT("INLINE pragma"),loc)
hsSigDoc (NoInlineSig  _ _  loc)      = (SLIT("NOINLINE pragma"),loc)
hsSigDoc (SpecInstSig _ loc)	      = (SLIT("SPECIALISE instance pragma"),loc)
hsSigDoc (FixSig (FixitySig _ _ loc)) = (SLIT("fixity declaration"), loc)
\end{code}

\begin{code}
instance (Outputable name) => Outputable (Sig name) where
    ppr sig = ppr_sig sig

ppr_sig :: Outputable name => Sig name -> SDoc
ppr_sig (Sig var ty _)
      = sep [ppr var <+> dcolon, nest 4 (ppr ty)]

ppr_sig (ClassOpSig var _ dm ty _)
      = sep [ppr var <+> pp_dm <+> dcolon, nest 4 (ppr ty)]
      where
	pp_dm = if dm then equals else empty	-- Default-method indicator

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


instance Outputable name => Outputable (FixitySig name) where
  ppr (FixitySig name fixity loc) = sep [ppr fixity, ppr name]

ppr_phase :: Maybe Int -> SDoc
ppr_phase Nothing  = empty
ppr_phase (Just n) = int n
\end{code}

Checking for distinct signatures; oh, so boring


\begin{code}
eqHsSig :: Sig Name -> Sig Name -> Bool
eqHsSig (Sig n1 _ _)         (Sig n2 _ _)         = n1 == n2
eqHsSig (InlineSig n1 _ _)   (InlineSig n2 _ _)   = n1 == n2
eqHsSig (NoInlineSig n1 _ _) (NoInlineSig n2 _ _) = n1 == n2

eqHsSig (SpecInstSig ty1 _)  (SpecInstSig ty2 _)  = ty1 == ty2
eqHsSig (SpecSig n1 ty1 _)   (SpecSig n2 ty2 _) 
  = -- may have many specialisations for one value;
    -- but not ones that are exactly the same...
    (n1 == n2) && (ty1 == ty2)

eqHsSig other_1 other_2 = False
\end{code}
