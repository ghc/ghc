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
import Type		( Type )
import Name		( Name )
import NameSet		( NameSet, elemNameSet )
import BasicTypes	( IPName, RecFlag(..), InlineSpec(..), Fixity )
import Outputable	
import SrcLoc		( Located(..), SrcSpan, unLoc )
import Util		( sortLe )
import Var		( TyVar, DictId, Id )
import Bag		( Bag, emptyBag, isEmptyBag, bagToList, unionBags, unionManyBags )
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

  | ValBindsOut				-- After renaming
	[(RecFlag, LHsBinds id)]	-- Dependency analysed
	[LSig Name]

type LHsBinds id  = Bag (LHsBind id)
type DictBinds id = LHsBinds id		-- Used for dictionary or method bindings
type LHsBind  id  = Located (HsBind id)

data HsBind id
  = FunBind {	-- FunBind is used for both functions 	f x = e
		-- and variables			f = \x -> e
-- Reason 1: the Match stuff lets us have an optional
--	   result type sig	f :: a->a = ...mentions a...
--
-- Reason 2: Special case for type inference: see TcBinds.tcMonoBinds
--
-- Reason 3: instance decls can only have FunBinds, which is convenient
--	     If you change this, you'll need tochange e.g. rnMethodBinds

	fun_id :: Located id,

	fun_infix :: Bool,	-- True => infix declaration

	fun_matches :: MatchGroup id,	-- The payload

	fun_co_fn :: ExprCoFn,	-- Coercion from the type of the MatchGroup to the type of
				-- the Id.  Example:
				--	f :: Int -> forall a. a -> a
				--	f x y = y
				-- Then the MatchGroup will have type (Int -> a' -> a')
				-- (with a free type variable a').  The coercion will take
				-- a CoreExpr of this type and convert it to a CoreExpr of
				-- type 	Int -> forall a'. a' -> a'
				-- Notice that the coercion captures the free a'.  That's
				-- why coercions are (CoreExpr -> CoreExpr), rather than
				-- just CoreExpr (with a functional type)

	bind_fvs :: NameSet	-- After the renamer, this contains a superset of the 
				-- Names of the other binders in this binding group that 
				-- are free in the RHS of the defn
				-- Before renaming, and after typechecking, 
				-- the field is unused; it's just an error thunk
    }

  | PatBind {	-- The pattern is never a simple variable;
		-- That case is done by FunBind
	pat_lhs    :: LPat id,
	pat_rhs    :: GRHSs id,
	pat_rhs_ty :: PostTcType,	-- Type of the GRHSs
	bind_fvs   :: NameSet		-- Same as for FunBind
    }

  | VarBind {	-- Dictionary binding and suchlike 
	var_id :: id,		-- All VarBinds are introduced by the type checker
	var_rhs :: LHsExpr id	-- Located only for consistency
    }

  | AbsBinds {					-- Binds abstraction; TRANSLATION
	abs_tvs     :: [TyVar],  
	abs_dicts   :: [DictId],
	abs_exports :: [([TyVar], id, id, [Prag])],	-- (tvs, poly_id, mono_id, prags)
	abs_binds   :: LHsBinds id		-- The dictionary bindings and typechecked user bindings
						-- mixed up together; you can tell the dict bindings because
						-- they are all VarBinds
    }
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
   = pprValBindsForUser binds sigs

  ppr (ValBindsOut sccs sigs) 
    = getPprStyle $ \ sty ->
      if debugStyle sty then	-- Print with sccs showing
	vcat (map ppr sigs) $$ vcat (map ppr_scc sccs)
     else
	pprValBindsForUser (unionManyBags (map snd sccs)) sigs
   where
     ppr_scc (rec_flag, binds) = pp_rec rec_flag <+> pprLHsBinds binds
     pp_rec Recursive    = ptext SLIT("rec")
     pp_rec NonRecursive = ptext SLIT("nonrec")

--  *not* pprLHsBinds because we don't want braces; 'let' and
-- 'where' include a list of HsBindGroups and we don't want
-- several groups of bindings each with braces around.
-- Sort by location before printing
pprValBindsForUser binds sigs
  = vcat (map snd (sort_by_loc decls))
  where

    decls :: [(SrcSpan, SDoc)]
    decls = [(loc, ppr sig)  | L loc sig <- sigs] ++
    	    [(loc, ppr bind) | L loc bind <- bagToList binds]

    sort_by_loc decls = sortLe (\(l1,_) (l2,_) -> l1 <= l2) decls

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
isEmptyValBinds (ValBindsIn ds sigs)  = isEmptyLHsBinds ds && null sigs
isEmptyValBinds (ValBindsOut ds sigs) = null ds && null sigs

emptyValBindsIn, emptyValBindsOut :: HsValBinds a
emptyValBindsIn  = ValBindsIn emptyBag []
emptyValBindsOut = ValBindsOut []      []

emptyLHsBinds :: LHsBinds id
emptyLHsBinds = emptyBag

isEmptyLHsBinds :: LHsBinds id -> Bool
isEmptyLHsBinds = isEmptyBag

------------
plusHsValBinds :: HsValBinds a -> HsValBinds a -> HsValBinds a
plusHsValBinds (ValBindsIn ds1 sigs1) (ValBindsIn ds2 sigs2)
  = ValBindsIn (ds1 `unionBags` ds2) (sigs1 ++ sigs2)
plusHsValBinds (ValBindsOut ds1 sigs1) (ValBindsOut ds2 sigs2)
  = ValBindsOut (ds1 ++ ds2) (sigs1 ++ sigs2)
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

ppr_monobind (PatBind { pat_lhs = pat, pat_rhs = grhss })      = pprPatBind pat grhss
ppr_monobind (VarBind { var_id = var, var_rhs = rhs })         = ppr var <+> equals <+> pprExpr (unLoc rhs)
ppr_monobind (FunBind { fun_id = fun, fun_matches = matches }) = pprFunBind (unLoc fun) matches
      -- ToDo: print infix if appropriate

ppr_monobind (AbsBinds { abs_tvs = tyvars, abs_dicts = dictvars, 
			 abs_exports = exports, abs_binds = val_binds })
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
\subsection{Coercion functions}
%*									*
%************************************************************************

\begin{code}
-- A Coercion is an expression with a hole in it
-- We need coercions to have concrete form so that we can zonk them

data ExprCoFn
  = CoHole			-- The identity coercion
  | CoCompose ExprCoFn ExprCoFn
  | CoApps ExprCoFn [Id]		-- Non-empty list
  | CoTyApps ExprCoFn [Type]		--   in all of these
  | CoLams [Id] ExprCoFn		--   so that the identity coercion
  | CoTyLams [TyVar] ExprCoFn		--   is just Hole
  | CoLet (LHsBinds Id) ExprCoFn	-- Would be nicer to be core bindings

(<.>) :: ExprCoFn -> ExprCoFn -> ExprCoFn
(<.>) = CoCompose

idCoercion :: ExprCoFn
idCoercion = CoHole

isIdCoercion :: ExprCoFn -> Bool
isIdCoercion CoHole = True
isIdCoercion other  = False
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
  = TypeSig	(Located name)	-- A bog-std type signature
		(LHsType name)

  | SpecSig 	(Located name)	-- Specialise a function or datatype ...
		(LHsType name)	-- ... to these types
		InlineSpec

  | InlineSig	(Located name)	-- Function name
		InlineSpec

  | SpecInstSig (LHsType name)	-- (Class tys); should be a specialisation of the 
				-- current instance decl

  | FixSig	(FixitySig name)	-- Fixity declaration

type LFixitySig name = Located (FixitySig name)
data FixitySig name = FixitySig (Located name) Fixity 

-- A Prag conveys pragmas from the type checker to the desugarer
data Prag 
  = InlinePrag 
	InlineSpec

  | SpecPrag   
	(HsExpr Id)	-- An expression, of the given specialised type, which
	PostTcType 	-- specialises the polymorphic function
	[Id]		-- Dicts mentioned free in the expression
	InlineSpec 	-- Inlining spec for the specialised function

isInlinePrag (InlinePrag _) = True
isInlinePrag prag	    = False

isSpecPrag (SpecPrag _ _ _ _) = True
isSpecPrag prag		      = False
\end{code}

\begin{code}
okBindSig :: NameSet -> LSig Name -> Bool
okBindSig ns sig = sigForThisGroup ns sig

okHsBootSig :: LSig Name -> Bool
okHsBootSig (L _ (TypeSig  _ _)) = True
okHsBootSig (L _ (FixSig _)) 	 = True
okHsBootSig sig	      	     	 = False

okClsDclSig :: LSig Name -> Bool
okClsDclSig (L _ (SpecInstSig _)) = False
okClsDclSig sig 	          = True	-- All others OK

okInstDclSig :: NameSet -> LSig Name -> Bool
okInstDclSig ns lsig@(L _ sig) = ok ns sig
  where
    ok ns (TypeSig _ _)	  = False
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
    f (TypeSig   n _)          = Just (unLoc n)
    f (SpecSig   n _ _)        = Just (unLoc n)
    f (InlineSig n _)          = Just (unLoc n)
    f (FixSig (FixitySig n _)) = Just (unLoc n)
    f other			= Nothing

isFixityLSig :: LSig name -> Bool
isFixityLSig (L _ (FixSig {})) = True
isFixityLSig _	               = False

isVanillaLSig :: LSig name -> Bool
isVanillaLSig (L _(TypeSig {})) = True
isVanillaLSig sig	        = False

isSpecLSig :: LSig name -> Bool
isSpecLSig (L _(SpecSig {})) = True
isSpecLSig sig	     	     = False

isSpecInstLSig (L _ (SpecInstSig {})) = True
isSpecInstLSig sig	       	      = False

isPragLSig :: LSig name -> Bool
	-- Identifies pragmas 
isPragLSig (L _ (SpecSig {}))   = True
isPragLSig (L _ (InlineSig {})) = True
isPragLSig other		= False

isInlineLSig :: LSig name -> Bool
	-- Identifies inline pragmas 
isInlineLSig (L _ (InlineSig {})) = True
isInlineLSig other		  = False

hsSigDoc (TypeSig {}) 		= ptext SLIT("type signature")
hsSigDoc (SpecSig {})	 	= ptext SLIT("SPECIALISE pragma")
hsSigDoc (InlineSig _ spec)   	= ppr spec <+> ptext SLIT("pragma")
hsSigDoc (SpecInstSig {})	= ptext SLIT("SPECIALISE instance pragma")
hsSigDoc (FixSig {}) 		= ptext SLIT("fixity declaration")
\end{code}

Signature equality is used when checking for duplicate signatures

\begin{code}
eqHsSig :: LSig Name -> LSig Name -> Bool
eqHsSig (L _ (FixSig (FixitySig n1 _))) (L _ (FixSig (FixitySig n2 _))) = unLoc n1 == unLoc n2
eqHsSig (L _ (TypeSig n1 _))         	(L _ (TypeSig n2 _))            = unLoc n1 == unLoc n2
eqHsSig (L _ (InlineSig n1 s1))	(L _ (InlineSig n2 s2))    	        = s1 == s2 && unLoc n1 == unLoc n2
 	-- For specialisations, we don't have equality over
	-- HsType, so it's not convenient to spot duplicate 
	-- specialisations here.  Check for this later, when we're in Type land
eqHsSig _other1 _other2 = False
\end{code}

\begin{code}
instance (OutputableBndr name) => Outputable (Sig name) where
    ppr sig = ppr_sig sig

ppr_sig :: OutputableBndr name => Sig name -> SDoc
ppr_sig (TypeSig var ty)	  = pprVarSig (unLoc var) ty
ppr_sig (FixSig fix_sig) 	  = ppr fix_sig
ppr_sig (SpecSig var ty inl) 	  = pragBrackets (pprSpec var ty inl)
ppr_sig (InlineSig var inl)       = pragBrackets (ppr inl <+> ppr var)
ppr_sig (SpecInstSig ty) 	  = pragBrackets (ptext SLIT("SPECIALIZE instance") <+> ppr ty)

instance Outputable name => Outputable (FixitySig name) where
  ppr (FixitySig name fixity) = sep [ppr fixity, ppr name]

pragBrackets :: SDoc -> SDoc
pragBrackets doc = ptext SLIT("{-#") <+> doc <+> ptext SLIT("#-}") 

pprVarSig :: (Outputable id, Outputable ty) => id -> ty -> SDoc
pprVarSig var ty = sep [ppr var <+> dcolon, nest 2 (ppr ty)]

pprSpec :: (Outputable id, Outputable ty) => id -> ty -> InlineSpec -> SDoc
pprSpec var ty inl = sep [ptext SLIT("SPECIALIZE") <+> ppr inl <+> pprVarSig var ty]

pprPrag :: Outputable id => id -> Prag -> SDoc
pprPrag var (InlinePrag inl)         = ppr inl <+> ppr var
pprPrag var (SpecPrag expr ty _ inl) = pprSpec var ty inl
\end{code}
