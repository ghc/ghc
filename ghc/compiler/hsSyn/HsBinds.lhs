%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsBinds]{Abstract syntax: top-level bindings and signatures}

Datatype for: @BindGroup@, @Bind@, @Sig@, @Bind@.

\begin{code}
module HsBinds where

#include "HsVersions.h"

import {-# SOURCE #-} HsExpr ( HsExpr, pprExpr, LHsExpr,
			       MatchGroup, pprFunBind,
			       GRHSs, pprPatBind )
import {-# SOURCE #-} HsPat  ( LPat )

import HsTypes		( LHsType, PostTcType )
import Name		( Name )
import NameSet		( NameSet, elemNameSet, nameSetToList )
import BasicTypes	( IPName, RecFlag(..), Activation(..), Fixity )
import Outputable	
import SrcLoc		( Located(..), unLoc )
import Var		( TyVar )
import Bag		( Bag, emptyBag, isEmptyBag, bagToList )
\end{code}

%************************************************************************
%*									*
\subsection{Bindings: @BindGroup@}
%*									*
%************************************************************************

Global bindings (where clauses)

\begin{code}
data HsBindGroup id
  = HsBindGroup			-- A mutually recursive group
	(LHsBinds id)
	[LSig id]		-- Empty on typechecker output, Type Signatures
	RecFlag

  | HsIPBinds
	[LIPBind id]		-- Not allowed at top level

instance OutputableBndr id => Outputable (HsBindGroup id) where
  ppr (HsBindGroup binds sigs is_rec)
     = vcat [ppr_isrec,
     	     vcat (map ppr sigs),
	     vcat (map ppr (bagToList binds))
		--  *not* pprLHsBinds because we don't want braces; 'let' and
		-- 'where' include a list of HsBindGroups and we don't want
		-- several groups of bindings each with braces around.
       ]
     where
       ppr_isrec = getPprStyle $ \ sty -> 
		   if userStyle sty then empty else
		   case is_rec of
		   	Recursive    -> ptext SLIT("{- rec -}")
			NonRecursive -> ptext SLIT("{- nonrec -}")

  ppr (HsIPBinds ipbinds)
     = vcat (map ppr ipbinds)

-- -----------------------------------------------------------------------------
-- Implicit parameter bindings

type LIPBind id = Located (IPBind id)

-- | Implicit parameter bindings.
data IPBind id
  = IPBind
	(IPName id)
	(LHsExpr id)

instance (OutputableBndr id) => Outputable (IPBind id) where
    ppr (IPBind id rhs) = pprBndr LetBind id <+> equals <+> pprExpr (unLoc rhs)

-- -----------------------------------------------------------------------------

type LHsBinds id  = Bag (LHsBind id)
type DictBinds id = LHsBinds id		-- Used for dictionary or method bindings
type LHsBind  id  = Located (HsBind id)

emptyLHsBinds :: LHsBinds id
emptyLHsBinds = emptyBag

isEmptyLHsBinds :: LHsBinds id -> Bool
isEmptyLHsBinds = isEmptyBag

pprLHsBinds :: OutputableBndr id => LHsBinds id -> SDoc
pprLHsBinds binds 
  | isEmptyLHsBinds binds = empty
  | otherwise = lbrace <+> vcat (map ppr (bagToList binds)) <+> rbrace

data HsBind id
  = FunBind     (Located id)
			-- Used for both functions 	f x = e
			-- and variables		f = \x -> e
			-- Reason: the Match stuff lets us have an optional
			--	   result type sig	f :: a->a = ...mentions a...
			--
			-- This also means that instance decls can only have
			-- FunBinds, so if you change this, you'll need to
			-- change e.g. rnMethodBinds
		Bool	-- True => infix declaration
		(MatchGroup id)

  | PatBind     (LPat id)	-- The pattern is never a simple variable;
				-- That case is done by FunBind
		(GRHSs id)
		PostTcType	-- Type of the GRHSs

  | VarBind id (Located (HsExpr id))	-- Dictionary binding and suchlike;
					-- located only for consistency

  | AbsBinds				-- Binds abstraction; TRANSLATION
		[TyVar]	  		-- Type variables
		[id]			-- Dicts
		[([TyVar], id, id)]	-- (type variables, polymorphic, momonmorphic) triples
		NameSet			-- Set of *polymorphic* variables that have an INLINE pragma
		(LHsBinds id)	 	-- The "business end"

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

	tp = /\ [a,b] -> \ [d1,d2] -> letrec DBINDS and BIND 
				       in (fm,gm)

\begin{code}
instance OutputableBndr id => Outputable (HsBind id) where
    ppr mbind = ppr_monobind mbind

ppr_monobind :: OutputableBndr id => HsBind id -> SDoc

ppr_monobind (PatBind pat grhss ty)    = pprPatBind pat grhss
ppr_monobind (VarBind var rhs)         = ppr var <+> equals <+> pprExpr (unLoc rhs)
ppr_monobind (FunBind fun inf matches) = pprFunBind (unLoc fun) matches
      -- ToDo: print infix if appropriate

ppr_monobind (AbsBinds tyvars dictvars exports inlines val_binds)
     = sep [ptext SLIT("AbsBinds"),
	    brackets (interpp'SP tyvars),
	    brackets (interpp'SP dictvars),
	    brackets (sep (punctuate comma (map ppr exports))),
	    brackets (interpp'SP (nameSetToList inlines))]
       $$
       nest 4 ( vcat [pprBndr LetBind x | (_,x,_) <- exports]
			-- Print type signatures
		$$
		pprLHsBinds val_binds )
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
type LSig name = Located (Sig name)

data Sig name
  = Sig		(Located name)	-- a bog-std type signature
		(LHsType name)

  | SpecSig 	(Located name)	-- specialise a function or datatype ...
		(LHsType name)	-- ... to these types

  | InlineSig	Bool		-- True <=> INLINE f, False <=> NOINLINE f
	 	(Located name)	-- Function name
		Activation	-- When inlining is *active*

  | SpecInstSig (LHsType name)	-- (Class tys); should be a specialisation of the 
				-- current instance decl

  | FixSig	(FixitySig name)	-- Fixity declaration

type LFixitySig name = Located (FixitySig name)
data FixitySig name = FixitySig (Located name) Fixity 
\end{code}

\begin{code}
okBindSig :: NameSet -> LSig Name -> Bool
okBindSig ns sig = sigForThisGroup ns sig

okClsDclSig :: LSig Name -> Bool
okClsDclSig (L _ (SpecInstSig _)) = False
okClsDclSig sig 	          = True	-- All others OK

okInstDclSig :: NameSet -> LSig Name -> Bool
okInstDclSig ns lsig@(L _ sig) = ok ns sig
  where
    ok ns (Sig _ _)	  = False
    ok ns (FixSig _)	  = False
    ok ns (SpecInstSig _) = True
    ok ns sig		  = sigForThisGroup ns lsig

sigForThisGroup :: NameSet -> LSig Name -> Bool
sigForThisGroup ns sig
  = case sigName sig of
	Nothing -> False
	Just n  -> n `elemNameSet` ns

sigName :: LSig name -> Maybe name
sigName (L _ sig) = f sig
 where
    f (Sig         n _)        = Just (unLoc n)
    f (SpecSig     n _)        = Just (unLoc n)
    f (InlineSig _ n _)        = Just (unLoc n)
    f (FixSig (FixitySig n _)) = Just (unLoc n)
    f other			= Nothing

isFixityLSig :: LSig name -> Bool
isFixityLSig (L _ (FixSig _)) = True
isFixityLSig _	              = False

isVanillaLSig :: LSig name -> Bool
isVanillaLSig (L _(Sig name _)) = True
isVanillaLSig sig	        = False

isPragLSig :: LSig name -> Bool
	-- Identifies pragmas 
isPragLSig (L _ (SpecSig _ _))     = True
isPragLSig (L _ (InlineSig _ _ _)) = True
isPragLSig (L _ (SpecInstSig _))   = True
isPragLSig other		   = False

hsSigDoc (Sig        _ _) 	  = ptext SLIT("type signature")
hsSigDoc (SpecSig    _ _) 	  = ptext SLIT("SPECIALISE pragma")
hsSigDoc (InlineSig True  _ _)    = ptext SLIT("INLINE pragma")
hsSigDoc (InlineSig False _ _)    = ptext SLIT("NOINLINE pragma")
hsSigDoc (SpecInstSig _)	  = ptext SLIT("SPECIALISE instance pragma")
hsSigDoc (FixSig (FixitySig _ _)) = ptext SLIT("fixity declaration")
\end{code}

Signature equality is used when checking for duplicate signatures

\begin{code}
eqHsSig :: Sig Name -> Sig Name -> Bool
eqHsSig (FixSig (FixitySig n1 _)) (FixSig (FixitySig n2 _)) = unLoc n1 == unLoc n2
eqHsSig (Sig n1 _)         	    (Sig n2 _)              = unLoc n1 == unLoc n2
eqHsSig (InlineSig b1 n1 _)	    (InlineSig b2 n2 _)     = b1 == b2 && unLoc n1 == unLoc n2
 	-- For specialisations, we don't have equality over
	-- HsType, so it's not convenient to spot duplicate 
	-- specialisations here.  Check for this later, when we're in Type land
eqHsSig _other1 _other2 = False
\end{code}

\begin{code}
instance (OutputableBndr name) => Outputable (Sig name) where
    ppr sig = ppr_sig sig

ppr_sig :: OutputableBndr name => Sig name -> SDoc
ppr_sig (Sig var ty)
      = sep [ppr var <+> dcolon, nest 4 (ppr ty)]

ppr_sig (SpecSig var ty)
      = sep [ hsep [text "{-# SPECIALIZE", ppr var, dcolon],
	      nest 4 (ppr ty <+> text "#-}")
	]

ppr_sig (InlineSig True var phase)
      = hsep [text "{-# INLINE", ppr phase, ppr var, text "#-}"]

ppr_sig (InlineSig False var phase)
      = hsep [text "{-# NOINLINE", ppr phase, ppr var, text "#-}"]

ppr_sig (SpecInstSig ty)
      = hsep [text "{-# SPECIALIZE instance", ppr ty, text "#-}"]

ppr_sig (FixSig fix_sig) = ppr fix_sig

instance Outputable name => Outputable (FixitySig name) where
  ppr (FixitySig name fixity) = sep [ppr fixity, ppr name]
\end{code}
