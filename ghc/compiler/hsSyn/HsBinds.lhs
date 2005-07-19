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
import NameSet		( NameSet, elemNameSet )
import BasicTypes	( IPName, RecFlag(..), Activation(..), Fixity )
import Outputable	
import SrcLoc		( Located(..), unLoc )
import Var		( TyVar, DictId, Id )
import Bag		( Bag, emptyBag, isEmptyBag, bagToList, unionBags )
\end{code}

%************************************************************************
%*									*
\subsection{Bindings: @BindGroup@}
%*									*
%************************************************************************

Global bindings (where clauses)

\begin{code}
data HsLocalBinds id	-- Bindings in a 'let' expression
			-- or a 'where' clause
  = HsValBinds (HsValBinds id)
  | HsIPBinds  (HsIPBinds id)
  | EmptyLocalBinds

data HsValBinds id	-- Value bindings (not implicit parameters)
  = ValBindsIn  			-- Before typechecking
	(LHsBinds id) [LSig id]		-- Not dependency analysed
					-- Recursive by default

  | ValBindsOut				-- After typechecking
	[(RecFlag, LHsBinds id)]	-- Dependency analysed


type LHsBinds id  = Bag (LHsBind id)
type DictBinds id = LHsBinds id		-- Used for dictionary or method bindings
type LHsBind  id  = Located (HsBind id)

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
		NameSet		-- After the renamer, this contains a superset of the 
				-- Names of the other binders in this binding group that 
				-- are free in the RHS of the defn
				-- Before renaming, and after typechecking, 
				-- the field is unused; it's just an error thunk

  | PatBind     (LPat id)	-- The pattern is never a simple variable;
				-- That case is done by FunBind
		(GRHSs id)
		PostTcType	-- Type of the GRHSs
		NameSet		-- Same as for FunBind

  | VarBind id (Located (HsExpr id))	-- Dictionary binding and suchlike 
					-- All VarBinds are introduced by the type checker
					-- Located only for consistency

  | AbsBinds					-- Binds abstraction; TRANSLATION
		[TyVar]	  			-- Type variables
		[DictId]			-- Dicts
		[([TyVar], id, id, [Prag])]	-- (tvs, poly_id, mono_id, prags)
		(LHsBinds id)	 		-- The dictionary bindings and typechecked user bindings
						-- mixed up together; you can tell the dict bindings because
						-- they are all VarBinds

	-- Consider (AbsBinds tvs ds [(ftvs, poly_f, mono_f) binds]
	-- 
	-- Creates bindings for (polymorphic, overloaded) poly_f
	-- in terms of monomorphic, non-overloaded mono_f
	--
	-- Invariants: 
	--	1. 'binds' binds mono_f
	--	2. ftvs is a subset of tvs
	--	3. ftvs includes all tyvars free in ds
	--
	-- See section 9 of static semantics paper for more details.
	-- (You can get a PhD for explaining the True Meaning
	--  of this last construct.)

placeHolderNames :: NameSet
-- Used for the NameSet in FunBind and PatBind prior to the renamer
placeHolderNames = panic "placeHolderNames"

------------
instance OutputableBndr id => Outputable (HsLocalBinds id) where
  ppr (HsValBinds bs) = ppr bs
  ppr (HsIPBinds bs)  = ppr bs
  ppr EmptyLocalBinds = empty

instance OutputableBndr id => Outputable (HsValBinds id) where
  ppr (ValBindsIn binds sigs)
   = vcat [vcat (map ppr sigs),
	   vcat (map ppr (bagToList binds))
		--  *not* pprLHsBinds because we don't want braces; 'let' and
		-- 'where' include a list of HsBindGroups and we don't want
		-- several groups of bindings each with braces around.
       ]
  ppr (ValBindsOut sccs) = vcat (map ppr_scc sccs)
     where
       ppr_scc (rec_flag, binds) = pp_rec rec_flag <+> pprLHsBinds binds
       pp_rec Recursive    = ptext SLIT("rec")
       pp_rec NonRecursive = ptext SLIT("nonrec")

pprLHsBinds :: OutputableBndr id => LHsBinds id -> SDoc
pprLHsBinds binds 
  | isEmptyLHsBinds binds = empty
  | otherwise = lbrace <+> vcat (map ppr (bagToList binds)) <+> rbrace

------------
emptyLocalBinds :: HsLocalBinds a
emptyLocalBinds = EmptyLocalBinds

isEmptyLocalBinds :: HsLocalBinds a -> Bool
isEmptyLocalBinds (HsValBinds ds) = isEmptyValBinds ds
isEmptyLocalBinds (HsIPBinds ds)  = isEmptyIPBinds ds
isEmptyLocalBinds EmptyLocalBinds = True

isEmptyValBinds :: HsValBinds a -> Bool
isEmptyValBinds (ValBindsIn ds sigs) = isEmptyLHsBinds ds && null sigs
isEmptyValBinds (ValBindsOut ds)     = null ds

emptyValBindsIn, emptyValBindsOut :: HsValBinds a
emptyValBindsIn  = ValBindsIn emptyBag []
emptyValBindsOut = ValBindsOut []

emptyLHsBinds :: LHsBinds id
emptyLHsBinds = emptyBag

isEmptyLHsBinds :: LHsBinds id -> Bool
isEmptyLHsBinds = isEmptyBag

------------
plusHsValBinds :: HsValBinds a -> HsValBinds a -> HsValBinds a
plusHsValBinds (ValBindsIn ds1 sigs1) (ValBindsIn ds2 sigs2)
  = ValBindsIn (ds1 `unionBags` ds2) (sigs1 ++ sigs2)
plusHsValBinds (ValBindsOut ds1) (ValBindsOut ds2)
  = ValBindsOut (ds1 ++ ds2)
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

ppr_monobind (PatBind pat grhss _ _)     = pprPatBind pat grhss
ppr_monobind (VarBind var rhs)           = ppr var <+> equals <+> pprExpr (unLoc rhs)
ppr_monobind (FunBind fun inf matches _) = pprFunBind (unLoc fun) matches
      -- ToDo: print infix if appropriate

ppr_monobind (AbsBinds tyvars dictvars exports val_binds)
     = sep [ptext SLIT("AbsBinds"),
	    brackets (interpp'SP tyvars),
	    brackets (interpp'SP dictvars),
	    brackets (sep (punctuate comma (map ppr_exp exports)))]
       $$
       nest 2 ( vcat [pprBndr LetBind x | (_,x,_,_) <- exports]
			-- Print type signatures
		$$ pprLHsBinds val_binds )
  where
    ppr_exp (tvs, gbl, lcl, prags)
	= vcat [ppr gbl <+> ptext SLIT("<=") <+> ppr tvs <+> ppr lcl,
	  	nest 2 (vcat (map (pprPrag gbl) prags))]
\end{code}

%************************************************************************
%*									*
		Implicit parameter bindings
%*									*
%************************************************************************

\begin{code}
data HsIPBinds id
  = IPBinds 
	[LIPBind id] 
	(DictBinds id)	-- Only in typechecker output; binds 
			-- uses of the implicit parameters

isEmptyIPBinds :: HsIPBinds id -> Bool
isEmptyIPBinds (IPBinds is ds) = null is && isEmptyBag ds

type LIPBind id = Located (IPBind id)

-- | Implicit parameter bindings.
data IPBind id
  = IPBind
	(IPName id)
	(LHsExpr id)

instance (OutputableBndr id) => Outputable (HsIPBinds id) where
  ppr (IPBinds bs ds) = vcat (map ppr bs) 
			$$ pprLHsBinds ds

instance (OutputableBndr id) => Outputable (IPBind id) where
  ppr (IPBind id rhs) = pprBndr LetBind id <+> equals <+> pprExpr (unLoc rhs)
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

-- A Prag conveys pragmas from the type checker to the desugarer
data Prag 
  = InlinePrag 	
	Bool 		-- True <=> INLINE, False <=> NOINLINE
	Activation

  | SpecPrag   
	(HsExpr Id)	-- An expression, of the given specialised type, which
	PostTcType 	-- specialises the polymorphic function
	[Id]		-- Dicts mentioned free in the expression

isInlinePrag (InlinePrag _ _) = True
isInlinePrag prag	      = False

isSpecPrag (SpecPrag _ _ _) = True
isSpecPrag prag		    = False
\end{code}

\begin{code}
okBindSig :: NameSet -> LSig Name -> Bool
okBindSig ns sig = sigForThisGroup ns sig

okHsBootSig :: LSig Name -> Bool
okHsBootSig (L _ (Sig  _ _)) = True
okHsBootSig (L _ (FixSig _)) = True
okHsBootSig sig	      	     = False

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

isSpecLSig :: LSig name -> Bool
isSpecLSig (L _(SpecSig name _)) = True
isSpecLSig sig	     		 = False

isSpecInstLSig (L _ (SpecInstSig _)) = True
isSpecInstLSig sig	       	     = False

isPragLSig :: LSig name -> Bool
	-- Identifies pragmas 
isPragLSig (L _ (SpecSig _ _))     = True
isPragLSig (L _ (InlineSig _ _ _)) = True
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
eqHsSig :: LSig Name -> LSig Name -> Bool
eqHsSig (L _ (FixSig (FixitySig n1 _))) (L _ (FixSig (FixitySig n2 _))) = unLoc n1 == unLoc n2
eqHsSig (L _ (Sig n1 _))         	(L _ (Sig n2 _))                = unLoc n1 == unLoc n2
eqHsSig (L _ (InlineSig b1 n1 _))	(L _ (InlineSig b2 n2 _))       = b1 == b2 && unLoc n1 == unLoc n2
 	-- For specialisations, we don't have equality over
	-- HsType, so it's not convenient to spot duplicate 
	-- specialisations here.  Check for this later, when we're in Type land
eqHsSig _other1 _other2 = False
\end{code}

\begin{code}
instance (OutputableBndr name) => Outputable (Sig name) where
    ppr sig = ppr_sig sig

ppr_sig :: OutputableBndr name => Sig name -> SDoc
ppr_sig (Sig var ty)		  = pprVarSig (unLoc var) ty
ppr_sig (FixSig fix_sig) 	  = ppr fix_sig
ppr_sig (SpecSig var ty) 	  = pragBrackets (pprSpec var ty)
ppr_sig (InlineSig inl var phase) = pragBrackets (pprInline var inl phase)
ppr_sig (SpecInstSig ty) 	  = pragBrackets (ptext SLIT("SPECIALIZE instance") <+> ppr ty)

instance Outputable name => Outputable (FixitySig name) where
  ppr (FixitySig name fixity) = sep [ppr fixity, ppr name]

pragBrackets :: SDoc -> SDoc
pragBrackets doc = ptext SLIT("{-#") <+> doc <+> ptext SLIT("#-}") 

pprInline :: Outputable id => id -> Bool -> Activation -> SDoc
pprInline var True  phase = hsep [ptext SLIT("INLINE"),   ppr phase, ppr var]
pprInline var False phase = hsep [ptext SLIT("NOINLINE"), ppr phase, ppr var]

pprVarSig :: (Outputable id, Outputable ty) => id -> ty -> SDoc
pprVarSig var ty = sep [ppr var <+> dcolon, nest 2 (ppr ty)]

pprSpec :: (Outputable id, Outputable ty) => id -> ty -> SDoc
pprSpec var ty = sep [ptext SLIT("SPECIALIZE") <+> pprVarSig var ty]

pprPrag :: Outputable id => id -> Prag -> SDoc
pprPrag var (InlinePrag inl act) = pprInline var inl act
pprPrag var (SpecPrag expr ty _) = pprSpec var ty
\end{code}
