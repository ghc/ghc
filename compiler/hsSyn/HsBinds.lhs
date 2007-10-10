%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsBinds]{Abstract syntax: top-level bindings and signatures}

Datatype for: @BindGroup@, @Bind@, @Sig@, @Bind@.

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module HsBinds where

#include "HsVersions.h"

import {-# SOURCE #-} HsExpr ( HsExpr, pprExpr, LHsExpr,
			       MatchGroup, pprFunBind,
			       GRHSs, pprPatBind )
import {-# SOURCE #-} HsPat  ( LPat )

import HsTypes
import PprCore ()
import Coercion
import Type
import Name
import NameSet
import BasicTypes
import Outputable	
import SrcLoc
import Util
import Var
import Bag
\end{code}

%************************************************************************
%*									*
\subsection{Bindings: @BindGroup@}
%*									*
%************************************************************************

Global bindings (where clauses)

\begin{code}
-- During renaming, we need bindings where the left-hand sides
-- have been renamed but the the right-hand sides have not.
-- the ...LR datatypes are parametrized by two id types,
-- one for the left and one for the right.
-- Other than during renaming, these will be the same.

type HsLocalBinds id = HsLocalBindsLR id id

data HsLocalBindsLR idL idR	-- Bindings in a 'let' expression
			       -- or a 'where' clause
  = HsValBinds (HsValBindsLR idL idR)
  | HsIPBinds  (HsIPBinds idR)
  | EmptyLocalBinds

type HsValBinds id = HsValBindsLR id id

data HsValBindsLR idL idR  -- Value bindings (not implicit parameters)
  = ValBindsIn             -- Before typechecking
	(LHsBindsLR idL idR) [LSig idR]	-- Not dependency analysed
					-- Recursive by default

  | ValBindsOut		       -- After renaming
	[(RecFlag, LHsBinds idL)]	-- Dependency analysed, later bindings 
                                        -- in the list may depend on earlier
                                        -- ones.
	[LSig Name]

type LHsBinds id  = Bag (LHsBind id)
type DictBinds id = LHsBinds id		-- Used for dictionary or method bindings
type LHsBind  id  = Located (HsBind id)
type HsBind id = HsBindLR id id

type LHsBindLR idL idR = Located (HsBindLR idL idR)
type LHsBindsLR idL idR = Bag (LHsBindLR idL idR)

data HsBindLR idL idR
  = FunBind {	-- FunBind is used for both functions 	f x = e
		-- and variables			f = \x -> e
-- Reason 1: Special case for type inference: see TcBinds.tcMonoBinds
--
-- Reason 2: instance decls can only have FunBinds, which is convenient
--	     If you change this, you'll need tochange e.g. rnMethodBinds

-- But note that the form	f :: a->a = ...
-- parses as a pattern binding, just like
--			(f :: a -> a) = ... 

	fun_id :: Located idL,

	fun_infix :: Bool,	-- True => infix declaration

	fun_matches :: MatchGroup idR,	-- The payload

	fun_co_fn :: HsWrapper,	-- Coercion from the type of the MatchGroup to the type of
				-- the Id.  Example:
				--	f :: Int -> forall a. a -> a
				--	f x y = y
				-- Then the MatchGroup will have type (Int -> a' -> a')
				-- (with a free type variable a').  The coercion will take
				-- a CoreExpr of this type and convert it to a CoreExpr of
				-- type 	Int -> forall a'. a' -> a'
				-- Notice that the coercion captures the free a'.

	bind_fvs :: NameSet,	-- After the renamer, this contains a superset of the 
				-- Names of the other binders in this binding group that 
				-- are free in the RHS of the defn
				-- Before renaming, and after typechecking, 
				-- the field is unused; it's just an error thunk

        fun_tick :: Maybe (Int,[idR])   -- This is the (optional) module-local tick number. 
    }

  | PatBind {	-- The pattern is never a simple variable;
		-- That case is done by FunBind
	pat_lhs    :: LPat idL,
	pat_rhs    :: GRHSs idR,
	pat_rhs_ty :: PostTcType,	-- Type of the GRHSs
	bind_fvs   :: NameSet		-- Same as for FunBind
    }

  | VarBind {	-- Dictionary binding and suchlike 
	var_id :: idL,		-- All VarBinds are introduced by the type checker
	var_rhs :: LHsExpr idR	-- Located only for consistency
    }

  | AbsBinds {					-- Binds abstraction; TRANSLATION
       abs_tvs     :: [TyVar],  
	abs_dicts   :: [DictId],
       -- AbsBinds only gets used when idL = idR after renaming,
       -- but these need to be idL's for the collect... code in HsUtil to have
       -- the right type
	abs_exports :: [([TyVar], idL, idL, [LPrag])],	-- (tvs, poly_id, mono_id, prags)
	abs_binds   :: LHsBinds idL		-- The dictionary bindings and typechecked user bindings
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
instance (OutputableBndr idL, OutputableBndr idR) => Outputable (HsLocalBindsLR idL idR) where
  ppr (HsValBinds bs) = ppr bs
  ppr (HsIPBinds bs)  = ppr bs
  ppr EmptyLocalBinds = empty

instance (OutputableBndr idL, OutputableBndr idR) => Outputable (HsValBindsLR idL idR) where
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
pprValBindsForUser :: (OutputableBndr idL, OutputableBndr idR, OutputableBndr id2)
		   => LHsBindsLR idL idR -> [LSig id2] -> SDoc
pprValBindsForUser binds sigs
  = pprDeeperList vcat (map snd (sort_by_loc decls))
  where

    decls :: [(SrcSpan, SDoc)]
    decls = [(loc, ppr sig)  | L loc sig <- sigs] ++
    	     [(loc, ppr bind) | L loc bind <- bagToList binds]

    sort_by_loc decls = sortLe (\(l1,_) (l2,_) -> l1 <= l2) decls

pprLHsBinds :: (OutputableBndr idL, OutputableBndr idR) => LHsBindsLR idL idR -> SDoc
pprLHsBinds binds 
  | isEmptyLHsBinds binds = empty
  | otherwise = lbrace <+> pprDeeperList vcat (map ppr (bagToList binds)) <+> rbrace

------------
emptyLocalBinds :: HsLocalBindsLR a b
emptyLocalBinds = EmptyLocalBinds

isEmptyLocalBinds :: HsLocalBindsLR a b -> Bool
isEmptyLocalBinds (HsValBinds ds) = isEmptyValBinds ds
isEmptyLocalBinds (HsIPBinds ds)  = isEmptyIPBinds ds
isEmptyLocalBinds EmptyLocalBinds = True

isEmptyValBinds :: HsValBindsLR a b -> Bool
isEmptyValBinds (ValBindsIn ds sigs)  = isEmptyLHsBinds ds && null sigs
isEmptyValBinds (ValBindsOut ds sigs) = null ds && null sigs

emptyValBindsIn, emptyValBindsOut :: HsValBindsLR a b
emptyValBindsIn  = ValBindsIn emptyBag []
emptyValBindsOut = ValBindsOut []      []

emptyLHsBinds :: LHsBindsLR idL idR
emptyLHsBinds = emptyBag

isEmptyLHsBinds :: LHsBindsLR idL idR -> Bool
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
instance (OutputableBndr idL, OutputableBndr idR) => Outputable (HsBindLR idL idR) where
    ppr mbind = ppr_monobind mbind

ppr_monobind :: (OutputableBndr idL, OutputableBndr idR) => HsBindLR idL idR -> SDoc

ppr_monobind (PatBind { pat_lhs = pat, pat_rhs = grhss })      = pprPatBind pat grhss
ppr_monobind (VarBind { var_id = var, var_rhs = rhs })         = ppr var <+> equals <+> pprExpr (unLoc rhs)
ppr_monobind (FunBind { fun_id = fun, fun_infix = inf,
			fun_matches = matches,
		        fun_tick = tick }) = 
		           (case tick of 
			      Nothing -> empty
			      Just t  -> text "-- tick id = " <> ppr t
			   ) $$ pprFunBind (unLoc fun) inf matches

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
  ppr (IPBinds bs ds) = pprDeeperList vcat (map ppr bs) 
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
-- A HsWrapper is an expression with a hole in it
-- We need coercions to have concrete form so that we can zonk them

data HsWrapper
  = WpHole			-- The identity coercion

  | WpCompose HsWrapper HsWrapper	-- (\a1..an. []) `WpCompose` (\x1..xn. [])
				--	= (\a1..an \x1..xn. [])

  | WpCo Coercion		-- A cast:  [] `cast` co
				-- Guaranteedn not the identity coercion

  | WpApp Var			-- [] d		the 'd' is a type-class dictionary
  | WpTyApp Type		-- [] t		the 't' is a type or corecion
  | WpLam Id	 		-- \d. []	the 'd' is a type-class dictionary
  | WpTyLam TyVar 		-- \a. []	the 'a' is a type or coercion variable

	-- Non-empty bindings, so that the identity coercion
	-- is always exactly WpHole
  | WpLet (LHsBinds Id)		-- let binds in []
				-- (would be nicer to be core bindings)

instance Outputable HsWrapper where 
  ppr co_fn = pprHsWrapper (ptext SLIT("<>")) co_fn

pprHsWrapper :: SDoc -> HsWrapper -> SDoc
pprHsWrapper it wrap = 
    let 
        help it WpHole            = it
        help it (WpCompose f1 f2) = help (help it f2) f1
        help it (WpCo co)     = sep [it, nest 2 (ptext SLIT("`cast`") <+> pprParendType co)]
        help it (WpApp id)    = sep [it, nest 2 (ppr id)]
        help it (WpTyApp ty)  = sep [it, ptext SLIT("@") <+> pprParendType ty]
        help it (WpLam id)    = sep [ptext SLIT("\\") <> pprBndr LambdaBind id <> dot, it]
        help it (WpTyLam tv)  = sep [ptext SLIT("/\\") <> pprBndr LambdaBind tv <> dot, it]
        help it (WpLet binds) = sep [ptext SLIT("let") <+> braces (ppr binds), it]
    in
      -- in debug mode, print the wrapper
      -- otherwise just print what's inside
      getPprStyle (\ s -> if debugStyle s then (help it wrap) else it)

(<.>) :: HsWrapper -> HsWrapper -> HsWrapper
WpHole <.> c = c
c <.> WpHole = c
c1 <.> c2    = c1 `WpCompose` c2

mkWpTyApps :: [Type] -> HsWrapper
mkWpTyApps tys = mk_co_fn WpTyApp (reverse tys)

mkWpApps :: [Id] -> HsWrapper
mkWpApps ids = mk_co_fn WpApp (reverse ids)

mkWpTyLams :: [TyVar] -> HsWrapper
mkWpTyLams ids = mk_co_fn WpTyLam ids

mkWpLams :: [Id] -> HsWrapper
mkWpLams ids = mk_co_fn WpLam ids

mk_co_fn :: (a -> HsWrapper) -> [a] -> HsWrapper
mk_co_fn f as = foldr (WpCompose . f) WpHole as

idHsWrapper :: HsWrapper
idHsWrapper = WpHole

isIdHsWrapper :: HsWrapper -> Bool
isIdHsWrapper WpHole = True
isIdHsWrapper other  = False
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

data Sig name	-- Signatures and pragmas
  = 	-- An ordinary type signature
	-- f :: Num a => a -> a
    TypeSig	(Located name)	-- A bog-std type signature
		(LHsType name)

	-- An ordinary fixity declaration
	--	infixl *** 8
  | FixSig	(FixitySig name)	-- Fixity declaration

	-- An inline pragma
	-- {#- INLINE f #-}
  | InlineSig	(Located name)	-- Function name
		InlineSpec

	-- A specialisation pragma
	-- {-# SPECIALISE f :: Int -> Int #-}
  | SpecSig 	(Located name)	-- Specialise a function or datatype ...
		(LHsType name)	-- ... to these types
		InlineSpec

	-- A specialisation pragma for instance declarations only
	-- {-# SPECIALISE instance Eq [Int] #-}
  | SpecInstSig (LHsType name)	-- (Class tys); should be a specialisation of the 
				-- current instance decl


type LFixitySig name = Located (FixitySig name)
data FixitySig name = FixitySig (Located name) Fixity 

-- A Prag conveys pragmas from the type checker to the desugarer
type LPrag = Located Prag
data Prag 
  = InlinePrag 
	InlineSpec

  | SpecPrag   
	(HsExpr Id)	-- An expression, of the given specialised type, which
	PostTcType 	-- specialises the polymorphic function
	[Id]		-- Dicts mentioned free in the expression
			--   Apr07: I think this is pretty useless
			--	    see Note [Const rule dicts] in DsBinds
	InlineSpec 	-- Inlining spec for the specialised function

isInlinePrag (InlinePrag _) = True
isInlinePrag prag	    = False

isSpecPrag (SpecPrag {}) = True
isSpecPrag prag		 = False
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
sigName (L _ sig) = sigNameNoLoc sig

sigNameNoLoc :: Sig name -> Maybe name    
sigNameNoLoc (TypeSig   n _)          = Just (unLoc n)
sigNameNoLoc (SpecSig   n _ _)        = Just (unLoc n)
sigNameNoLoc (InlineSig n _)          = Just (unLoc n)
sigNameNoLoc (FixSig (FixitySig n _)) = Just (unLoc n)
sigNameNoLoc other		                = Nothing

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

pprPrag :: Outputable id => id -> LPrag -> SDoc
pprPrag var (L _ (InlinePrag inl))         = ppr inl <+> ppr var
pprPrag var (L _ (SpecPrag expr ty _ inl)) = pprSpec var ty inl
\end{code}

