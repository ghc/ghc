%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[TypeRep]{Type - friends' interface}

\begin{code}
module TypeRep (
	TyThing(..), 
	Type(..), TyNote(..), 		-- Representation visible 
	PredType(..),	 		-- to friends
	
 	Kind, ThetaType,		-- Synonyms

	funTyCon,

	-- Pretty-printing
	pprType, pprParendType, pprTyThingCategory,
	pprPred, pprTheta, pprThetaArrow, pprClassPred,

	-- Kinds
	liftedTypeKind, unliftedTypeKind, openTypeKind,
        argTypeKind, ubxTupleKind,
	isLiftedTypeKindCon, isLiftedTypeKind,
	mkArrowKind, mkArrowKinds,

        -- Kind constructors...
        liftedTypeKindTyCon, openTypeKindTyCon, unliftedTypeKindTyCon,
        argTypeKindTyCon, ubxTupleKindTyCon,

        -- And their names
        unliftedTypeKindTyConName, openTypeKindTyConName,
        ubxTupleKindTyConName, argTypeKindTyConName,
        liftedTypeKindTyConName,

        -- Super Kinds
	tySuperKind, coSuperKind,
        isTySuperKind, isCoSuperKind,
	tySuperKindTyCon, coSuperKindTyCon,
        
        isCoercionKindTyCon,

	pprKind, pprParendKind
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} DataCon( DataCon, dataConName )
import Monad 	  ( guard )
-- friends:

import Var	  ( Var, Id, TyVar, tyVarKind )
import VarSet     ( TyVarSet )
import Name	  ( Name, NamedThing(..), BuiltInSyntax(..), mkWiredInName )
import OccName	  ( mkOccNameFS, tcName, parenSymOcc )
import BasicTypes ( IPName, tupleParens )
import TyCon	  ( TyCon, mkFunTyCon, tyConArity, tupleTyConBoxity, isTupleTyCon, isRecursiveTyCon, isNewTyCon, mkVoidPrimTyCon, mkSuperKindTyCon, isSuperKindTyCon, mkCoercionTyCon )
import Class	  ( Class )

-- others
import PrelNames  ( gHC_PRIM, funTyConKey, tySuperKindTyConKey, 
                    coSuperKindTyConKey, liftedTypeKindTyConKey,
                    openTypeKindTyConKey, unliftedTypeKindTyConKey,
                    ubxTupleKindTyConKey, argTypeKindTyConKey, listTyConKey, 
                    parrTyConKey, hasKey, eqCoercionKindTyConKey )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{Type Classifications}
%*									*
%************************************************************************

A type is

	*unboxed*	iff its representation is other than a pointer
			Unboxed types are also unlifted.

	*lifted*	A type is lifted iff it has bottom as an element.
			Closures always have lifted types:  i.e. any
			let-bound identifier in Core must have a lifted
			type.  Operationally, a lifted object is one that
			can be entered.

			Only lifted types may be unified with a type variable.

	*algebraic*	A type with one or more constructors, whether declared
			with "data" or "newtype".   
			An algebraic type is one that can be deconstructed
			with a case expression.  
			*NOT* the same as lifted types,  because we also 
			include unboxed tuples in this classification.

	*data*		A type declared with "data".  Also boxed tuples.

	*primitive*	iff it is a built-in type that can't be expressed
			in Haskell.

Currently, all primitive types are unlifted, but that's not necessarily
the case.  (E.g. Int could be primitive.)

Some primitive types are unboxed, such as Int#, whereas some are boxed
but unlifted (such as ByteArray#).  The only primitive types that we
classify as algebraic are the unboxed tuples.

examples of type classifications:

Type		primitive	boxed		lifted		algebraic    
-----------------------------------------------------------------------------
Int#,		Yes		No		No		No
ByteArray#	Yes		Yes		No		No
(# a, b #)	Yes		No		No		Yes
(  a, b  )	No		Yes		Yes		Yes
[a]		No		Yes		Yes		Yes



	----------------------
	A note about newtypes
	----------------------

Consider
	newtype N = MkN Int

Then we want N to be represented as an Int, and that's what we arrange.
The front end of the compiler [TcType.lhs] treats N as opaque, 
the back end treats it as transparent [Type.lhs].

There's a bit of a problem with recursive newtypes
	newtype P = MkP P
	newtype Q = MkQ (Q->Q)

Here the 'implicit expansion' we get from treating P and Q as transparent
would give rise to infinite types, which in turn makes eqType diverge.
Similarly splitForAllTys and splitFunTys can get into a loop.  

Solution: 

* Newtypes are always represented using TyConApp.

* For non-recursive newtypes, P, treat P just like a type synonym after 
  type-checking is done; i.e. it's opaque during type checking (functions
  from TcType) but transparent afterwards (functions from Type).  
  "Treat P as a type synonym" means "all functions expand NewTcApps 
  on the fly".

  Applications of the data constructor P simply vanish:
	P x = x
  

* For recursive newtypes Q, treat the Q and its representation as 
  distinct right through the compiler.  Applications of the data consructor
  use a coerce:
	Q = \(x::Q->Q). coerce Q x
  They are rare, so who cares if they are a tiny bit less efficient.

The typechecker (TcTyDecls) identifies enough type construtors as 'recursive'
to cut all loops.  The other members of the loop may be marked 'non-recursive'.


%************************************************************************
%*									*
\subsection{The data type}
%*									*
%************************************************************************


\begin{code}
data Type
  = TyVarTy TyVar	

  | AppTy
	Type		-- Function is *not* a TyConApp
	Type		-- It must be another AppTy, or TyVarTy
			-- (or NoteTy of these)

  | TyConApp		-- Application of a TyCon, including newtypes *and* synonyms
	TyCon		--  *Invariant* saturated appliations of FunTyCon and
			-- 	synonyms have their own constructors, below.
			-- However, *unsaturated* FunTyCons do appear as TyConApps.  
			-- 
	[Type]		-- Might not be saturated.
			-- Even type synonyms are not necessarily saturated;
			-- for example unsaturated type synonyms can appear as the 
			-- RHS of a type synonym.

  | FunTy		-- Special case of TyConApp: TyConApp FunTyCon [t1,t2]
	Type
	Type

  | ForAllTy		-- A polymorphic type
	TyVar
	Type	

  | PredTy		-- The type of evidence for a type predictate
	PredType	-- See Note [PredTy], and Note [Equality predicates]
	-- NB: A PredTy (EqPred _ _) can appear only as the kind
	--     of a coercion variable; never as the argument or result
	--     of a FunTy (unlike ClassP, IParam)

  | NoteTy 		-- A type with a note attached
	TyNote
	Type		-- The expanded version

type Kind = Type 	-- Invariant: a kind is always
			--	FunTy k1 k2
			-- or	TyConApp PrimTyCon [...]
			-- or	TyVar kv (during inference only)
			-- or   ForAll ... (for top-level coercions)

type SuperKind = Type   -- Invariant: a super kind is always 
                        --   TyConApp SuperKindTyCon ...

type Coercion = Type

type CoercionKind = Kind

data TyNote = FTVNote TyVarSet	-- The free type variables of the noted expression
\end{code}

-------------------------------------
 		Note [PredTy]

A type of the form
	PredTy p
represents a value whose type is the Haskell predicate p, 
where a predicate is what occurs before the '=>' in a Haskell type.
It can be expanded into its representation, but: 

	* The type checker must treat it as opaque
	* The rest of the compiler treats it as transparent

Consider these examples:
	f :: (Eq a) => a -> Int
	g :: (?x :: Int -> Int) => a -> Int
	h :: (r\l) => {r} => {l::Int | r}

Here the "Eq a" and "?x :: Int -> Int" and "r\l" are all called *predicates*
Predicates are represented inside GHC by PredType:

\begin{code}
data PredType 
  = ClassP Class [Type]		-- Class predicate
  | IParam (IPName Name) Type	-- Implicit parameter
  | EqPred Type Type		-- Equality predicate (ty1 :=: ty2)

type ThetaType = [PredType]
\end{code}

(We don't support TREX records yet, but the setup is designed
to expand to allow them.)

A Haskell qualified type, such as that for f,g,h above, is
represented using 
	* a FunTy for the double arrow
	* with a PredTy as the function argument

The predicate really does turn into a real extra argument to the
function.  If the argument has type (PredTy p) then the predicate p is
represented by evidence (a dictionary, for example, of type (predRepTy p).

Note [Equality predicates]
~~~~~~~~~~~~~~~~~~~~~~~~~~
	forall a b. (a :=: S b) => a -> b
could be represented by
	ForAllTy a (ForAllTy b (FunTy (PredTy (EqPred a (S b))) ...))
OR
	ForAllTy a (ForAllTy b (ForAllTy (c::PredTy (EqPred a (S b))) ...))

The latter is what we do.  (Unlike for class and implicit parameter
constraints, which do use FunTy.)

Reason:
	* FunTy is always a *value* function
	* ForAllTy is discarded at runtime

We often need to make a "wildcard" (c::PredTy..).  We always use the same
name (wildCoVarName), since it's not mentioned.


%************************************************************************
%*									*
			TyThing
%*									*
%************************************************************************

Despite the fact that DataCon has to be imported via a hi-boot route, 
this module seems the right place for TyThing, because it's needed for
funTyCon and all the types in TysPrim.

\begin{code}
data TyThing = AnId     Id
	     | ADataCon DataCon
	     | ATyCon   TyCon
	     | AClass   Class

instance Outputable TyThing where
  ppr thing = pprTyThingCategory thing <+> quotes (ppr (getName thing))

pprTyThingCategory :: TyThing -> SDoc
pprTyThingCategory (ATyCon _) 	= ptext SLIT("Type constructor")
pprTyThingCategory (AClass _)   = ptext SLIT("Class")
pprTyThingCategory (AnId   _)   = ptext SLIT("Identifier")
pprTyThingCategory (ADataCon _) = ptext SLIT("Data constructor")

instance NamedThing TyThing where	-- Can't put this with the type
  getName (AnId id)     = getName id	-- decl, because the DataCon instance
  getName (ATyCon tc)   = getName tc	-- isn't visible there
  getName (AClass cl)   = getName cl
  getName (ADataCon dc) = dataConName dc
\end{code}


%************************************************************************
%*									*
		Wired-in type constructors
%*									*
%************************************************************************

We define a few wired-in type constructors here to avoid module knots

\begin{code}
--------------------------
-- First the TyCons...

funTyCon = mkFunTyCon funTyConName (mkArrowKinds [argTypeKind, openTypeKind] liftedTypeKind)
	-- You might think that (->) should have type (?? -> ? -> *), and you'd be right
	-- But if we do that we get kind errors when saying
	--	instance Control.Arrow (->)
	-- becuase the expected kind is (*->*->*).  The trouble is that the
	-- expected/actual stuff in the unifier does not go contra-variant, whereas
	-- the kind sub-typing does.  Sigh.  It really only matters if you use (->) in
	-- a prefix way, thus:  (->) Int# Int#.  And this is unusual.


tySuperKindTyCon     = mkSuperKindTyCon tySuperKindTyConName
coSuperKindTyCon     = mkSuperKindTyCon coSuperKindTyConName

liftedTypeKindTyCon   = mkKindTyCon liftedTypeKindTyConName
openTypeKindTyCon     = mkKindTyCon openTypeKindTyConName
unliftedTypeKindTyCon = mkKindTyCon unliftedTypeKindTyConName
ubxTupleKindTyCon     = mkKindTyCon ubxTupleKindTyConName
argTypeKindTyCon      = mkKindTyCon argTypeKindTyConName
eqCoercionKindTyCon = 
  mkCoercionTyCon eqCoercionKindTyConName 2 (\ _ -> coSuperKind)

mkKindTyCon :: Name -> TyCon
mkKindTyCon name = mkVoidPrimTyCon name tySuperKind 0

--------------------------
-- ... and now their names

tySuperKindTyConName      = mkPrimTyConName FSLIT("BOX") tySuperKindTyConKey tySuperKindTyCon
coSuperKindTyConName      = mkPrimTyConName FSLIT("COERCION") coSuperKindTyConKey coSuperKindTyCon
liftedTypeKindTyConName   = mkPrimTyConName FSLIT("*") liftedTypeKindTyConKey liftedTypeKindTyCon
openTypeKindTyConName     = mkPrimTyConName FSLIT("?") openTypeKindTyConKey openTypeKindTyCon
unliftedTypeKindTyConName = mkPrimTyConName FSLIT("#") unliftedTypeKindTyConKey unliftedTypeKindTyCon
ubxTupleKindTyConName     = mkPrimTyConName FSLIT("(##)") ubxTupleKindTyConKey ubxTupleKindTyCon
argTypeKindTyConName      = mkPrimTyConName FSLIT("??") argTypeKindTyConKey argTypeKindTyCon
funTyConName              = mkPrimTyConName FSLIT("(->)") funTyConKey funTyCon

eqCoercionKindTyConName   = mkWiredInName gHC_PRIM (mkOccNameFS tcName (FSLIT(":=:"))) 
					eqCoercionKindTyConKey Nothing (ATyCon eqCoercionKindTyCon) 
					BuiltInSyntax
 
mkPrimTyConName occ key tycon = mkWiredInName gHC_PRIM (mkOccNameFS tcName occ) 
					      key 
					      Nothing 		-- No parent object
					      (ATyCon tycon)
					      BuiltInSyntax
	-- All of the super kinds and kinds are defined in Prim and use BuiltInSyntax,
	-- because they are never in scope in the source

------------------
-- We also need Kinds and SuperKinds, locally and in TyCon

kindTyConType :: TyCon -> Type
kindTyConType kind = TyConApp kind []

liftedTypeKind   = kindTyConType liftedTypeKindTyCon
unliftedTypeKind = kindTyConType unliftedTypeKindTyCon
openTypeKind     = kindTyConType openTypeKindTyCon
argTypeKind      = kindTyConType argTypeKindTyCon
ubxTupleKind	 = kindTyConType ubxTupleKindTyCon

mkArrowKind :: Kind -> Kind -> Kind
mkArrowKind k1 k2 = FunTy k1 k2

mkArrowKinds :: [Kind] -> Kind -> Kind
mkArrowKinds arg_kinds result_kind = foldr mkArrowKind result_kind arg_kinds

tySuperKind, coSuperKind :: SuperKind
tySuperKind = kindTyConType tySuperKindTyCon 
coSuperKind = kindTyConType coSuperKindTyCon 

isTySuperKind (NoteTy _ ty)    = isTySuperKind ty
isTySuperKind (TyConApp kc []) = kc `hasKey` tySuperKindTyConKey
isTySuperKind other            = False

isCoSuperKind :: SuperKind -> Bool
isCoSuperKind (NoteTy _ ty)    = isCoSuperKind ty
isCoSuperKind (TyConApp kc []) = kc `hasKey` coSuperKindTyConKey
isCoSuperKind other            = False

isCoercionKindTyCon kc = kc `hasKey` eqCoercionKindTyConKey


-------------------
-- lastly we need a few functions on Kinds

isLiftedTypeKindCon tc    = tc `hasKey` liftedTypeKindTyConKey

isLiftedTypeKind (TyConApp tc []) = isLiftedTypeKindCon tc
isLiftedTypeKind other            = False


\end{code}



%************************************************************************
%*									*
\subsection{The external interface}
%*									*
%************************************************************************

@pprType@ is the standard @Type@ printer; the overloaded @ppr@ function is
defined to use this.  @pprParendType@ is the same, except it puts
parens around the type, except for the atomic cases.  @pprParendType@
works just by setting the initial context precedence very high.

\begin{code}
data Prec = TopPrec 	-- No parens
	  | FunPrec 	-- Function args; no parens for tycon apps
	  | TyConPrec 	-- Tycon args; no parens for atomic
	  deriving( Eq, Ord )

maybeParen :: Prec -> Prec -> SDoc -> SDoc
maybeParen ctxt_prec inner_prec pretty
  | ctxt_prec < inner_prec = pretty
  | otherwise		   = parens pretty

------------------
pprType, pprParendType :: Type -> SDoc
pprType       ty = ppr_type TopPrec   ty
pprParendType ty = ppr_type TyConPrec ty

------------------
pprPred :: PredType -> SDoc
pprPred (ClassP cls tys) = pprClassPred cls tys
pprPred (IParam ip ty)   = ppr ip <> dcolon <> pprType ty
pprPred (EqPred ty1 ty2) = sep [ppr ty1, nest 2 (ptext SLIT(":=:")), ppr ty2]

pprClassPred :: Class -> [Type] -> SDoc
pprClassPred clas tys = parenSymOcc (getOccName clas) (ppr clas) 
			<+> sep (map pprParendType tys)

pprTheta :: ThetaType -> SDoc
pprTheta theta = parens (sep (punctuate comma (map pprPred theta)))

pprThetaArrow :: ThetaType -> SDoc
pprThetaArrow theta 
  | null theta = empty
  | otherwise  = parens (sep (punctuate comma (map pprPred theta))) <+> ptext SLIT("=>")

------------------
instance Outputable Type where
    ppr ty = pprType ty

instance Outputable PredType where
    ppr = pprPred

instance Outputable name => OutputableBndr (IPName name) where
    pprBndr _ n = ppr n	-- Simple for now

------------------
	-- OK, here's the main printer

pprKind = pprType
pprParendKind = pprParendType

ppr_type :: Prec -> Type -> SDoc
ppr_type p (TyVarTy tv)       = ppr tv
ppr_type p (PredTy pred)      = braces (ppr pred)
ppr_type p (NoteTy other ty2) = ppr_type p ty2
ppr_type p (TyConApp tc tys)  = ppr_tc_app p tc tys

ppr_type p (AppTy t1 t2) = maybeParen p TyConPrec $
			   pprType t1 <+> ppr_type TyConPrec t2

ppr_type p ty@(ForAllTy _ _)       = ppr_forall_type p ty
ppr_type p ty@(FunTy (PredTy _) _) = ppr_forall_type p ty

ppr_type p (FunTy ty1 ty2)
  = -- We don't want to lose synonyms, so we mustn't use splitFunTys here.
    maybeParen p FunPrec $
    sep (ppr_type FunPrec ty1 : ppr_fun_tail ty2)
  where
    ppr_fun_tail (FunTy ty1 ty2) = (arrow <+> ppr_type FunPrec ty1) : ppr_fun_tail ty2
    ppr_fun_tail other_ty        = [arrow <+> pprType other_ty]

ppr_forall_type :: Prec -> Type -> SDoc
ppr_forall_type p ty
  = maybeParen p FunPrec $
    sep [pprForAll tvs, pprThetaArrow ctxt, pprType tau]
  where
    (tvs,  rho) = split1 [] ty
    (ctxt, tau) = split2 [] rho

    split1 tvs (ForAllTy tv ty) = split1 (tv:tvs) ty
    split1 tvs (NoteTy _ ty)    = split1 tvs ty
    split1 tvs ty		= (reverse tvs, ty)
 
    split2 ps (NoteTy _ arg 	-- Rather a disgusting case
	       `FunTy` res) 	      = split2 ps (arg `FunTy` res)
    split2 ps (PredTy p `FunTy` ty)   = split2 (p:ps) ty
    split2 ps (NoteTy _ ty) 	      = split2 ps ty
    split2 ps ty		      = (reverse ps, ty)

ppr_tc_app :: Prec -> TyCon -> [Type] -> SDoc
ppr_tc_app p tc [] 
  = ppr_tc tc
ppr_tc_app p tc [ty] 
  | tc `hasKey` listTyConKey = brackets (pprType ty)
  | tc `hasKey` parrTyConKey = ptext SLIT("[:") <> pprType ty <> ptext SLIT(":]")
  | tc `hasKey` liftedTypeKindTyConKey   = ptext SLIT("*")
  | tc `hasKey` unliftedTypeKindTyConKey = ptext SLIT("#")
  | tc `hasKey` openTypeKindTyConKey     = ptext SLIT("(?)")
  | tc `hasKey` ubxTupleKindTyConKey     = ptext SLIT("(#)")
  | tc `hasKey` argTypeKindTyConKey      = ptext SLIT("??")

ppr_tc_app p tc tys
  | isTupleTyCon tc && tyConArity tc == length tys
  = tupleParens (tupleTyConBoxity tc) (sep (punctuate comma (map pprType tys)))
  | otherwise
  = maybeParen p TyConPrec $
    ppr_tc tc <+> sep (map (ppr_type TyConPrec) tys)

ppr_tc :: TyCon -> SDoc
ppr_tc tc = parenSymOcc (getOccName tc) (pp_nt_debug <> ppr tc)
  where
   pp_nt_debug | isNewTyCon tc = ifPprDebug (if isRecursiveTyCon tc 
				             then ptext SLIT("<recnt>")
					     else ptext SLIT("<nt>"))
	       | otherwise     = empty

-------------------
pprForAll []  = empty
pprForAll tvs = ptext SLIT("forall") <+> sep (map pprTvBndr tvs) <> dot

pprTvBndr tv | isLiftedTypeKind kind = ppr tv
	     | otherwise	     = parens (ppr tv <+> dcolon <+> pprKind kind)
	     where
	       kind = tyVarKind tv
\end{code}

