%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[TypeRep]{Type - friends' interface}

\begin{code}
module TypeRep (
	Type(..), TyNote(..), UsageAnn(..),		-- Representation visible to friends
	Kind, TyVarSubst,

	superKind, superBoxity,				-- :: SuperKind

	boxedKind,					-- :: Kind :: BX
	anyBoxKind,					-- :: Kind :: BX
	typeCon,					-- :: KindCon :: BX -> KX
	anyBoxCon,					-- :: KindCon :: BX

	boxedTypeKind, unboxedTypeKind, openTypeKind, 	-- Kind :: superKind

	mkArrowKind, mkArrowKinds,

	funTyCon
    ) where

#include "HsVersions.h"

-- friends:
import Var	( TyVar, UVar )
import VarEnv
import VarSet

import Name	( Provenance(..), ExportFlag(..),
		  mkWiredInTyConName, mkGlobalName, mkKindOccFS, tcName,
		)
import TyCon	( TyCon, KindCon,
		  mkFunTyCon, mkKindCon, mkSuperKindCon,
		)

-- others
import SrcLoc		( mkBuiltinSrcLoc )
import PrelMods		( pREL_GHC )
import Unique		-- quite a few *Keys
import Util		( thenCmp )
\end{code}

%************************************************************************
%*									*
\subsection{Type Classifications}
%*									*
%************************************************************************

A type is

	*unboxed*	iff its representation is other than a pointer
			Unboxed types cannot instantiate a type variable.
			Unboxed types are always unlifted.

	*lifted*	A type is lifted iff it has bottom as an element.
			Closures always have lifted types:  i.e. any
			let-bound identifier in Core must have a lifted
			type.  Operationally, a lifted object is one that
			can be entered.
			(NOTE: previously "pointed").			

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

%************************************************************************
%*									*
\subsection{The data type}
%*									*
%************************************************************************


\begin{code}
type SuperKind = Type
type Kind      = Type

type TyVarSubst = TyVarEnv Type

data Type
  = TyVarTy TyVar

  | AppTy
	Type		-- Function is *not* a TyConApp
	Type

  | TyConApp			-- Application of a TyCon
	TyCon			-- *Invariant* saturated appliations of FunTyCon and
				-- 	synonyms have their own constructors, below.
	[Type]		-- Might not be saturated.

  | FunTy			-- Special case of TyConApp: TyConApp FunTyCon [t1,t2]
	Type
	Type

  | NoteTy 			-- Saturated application of a type synonym
	TyNote
	Type		-- The expanded version

  | ForAllTy
	TyVar
	Type		-- TypeKind

data TyNote
  = SynNote Type	-- The unexpanded version of the type synonym; always a TyConApp
  | FTVNote TyVarSet	-- The free type variables of the noted expression
  | UsgNote UsageAnn    -- The usage annotation at this node
  | UsgForAll UVar      -- Annotation variable binder

data UsageAnn
  = UsOnce		-- Used at most once
  | UsMany		-- Used possibly many times (no info; this annotation can be omitted)
  | UsVar    UVar	-- Annotation is variable (unbound OK only inside analysis)
\end{code}


%************************************************************************
%*									*
\subsection{Kinds}
%*									*
%************************************************************************

Kinds
~~~~~
k::K = Type bx
     | k -> k
     | kv

kv :: KX is a kind variable

Type :: BX -> KX

bx::BX = Boxed 
      |  Unboxed
      |  AnyBox		-- Used *only* for special built-in things
			-- like error :: forall (a::*?). String -> a
			-- Here, the 'a' can be instantiated to a boxed or
			-- unboxed type.
      |  bv

bxv :: BX is a boxity variable

sk = KX		-- A kind
   | BX		-- A boxity
   | sk -> sk	-- In ptic (BX -> KX)

\begin{code}
mk_kind_name key str = mkGlobalName key pREL_GHC (mkKindOccFS tcName str)
				    (LocalDef mkBuiltinSrcLoc NotExported)
	-- mk_kind_name is a bit of a hack
	-- The LocalDef means that we print the name without
	-- a qualifier, which is what we want for these kinds.
	-- It's used for both Kinds and Boxities
\end{code}

Define KX, BX.

\begin{code}
superKind :: SuperKind 		-- KX, the type of all kinds
superKindName = mk_kind_name kindConKey SLIT("KX")
superKind = TyConApp (mkSuperKindCon superKindName) []

superBoxity :: SuperKind		-- BX, the type of all boxities
superBoxityName = mk_kind_name boxityConKey SLIT("BX")
superBoxity = TyConApp (mkSuperKindCon superBoxityName) []
\end{code}

Define Boxed, Unboxed, AnyBox

\begin{code}
boxedKind, unboxedKind, anyBoxKind :: Kind	-- Of superkind superBoxity

boxedConName = mk_kind_name boxedConKey SLIT("*")
boxedKind    = TyConApp (mkKindCon boxedConName superBoxity) []

unboxedConName = mk_kind_name unboxedConKey SLIT("#")
unboxedKind    = TyConApp (mkKindCon unboxedConName superBoxity) []

anyBoxConName = mk_kind_name anyBoxConKey SLIT("?")
anyBoxCon     = mkKindCon anyBoxConName superBoxity	-- A kind of wild card
anyBoxKind    = TyConApp anyBoxCon []
\end{code}

Define Type

\begin{code}
typeCon :: KindCon
typeConName = mk_kind_name typeConKey SLIT("Type")
typeCon     = mkKindCon typeConName (superBoxity `FunTy` superKind)
\end{code}

Define (Type Boxed), (Type Unboxed), (Type AnyBox)

\begin{code}
boxedTypeKind, unboxedTypeKind, openTypeKind :: Kind
boxedTypeKind   = TyConApp typeCon [boxedKind]
unboxedTypeKind = TyConApp typeCon [unboxedKind]
openTypeKind	= TyConApp typeCon [anyBoxKind]

mkArrowKind :: Kind -> Kind -> Kind
mkArrowKind k1 k2 = k1 `FunTy` k2

mkArrowKinds :: [Kind] -> Kind -> Kind
mkArrowKinds arg_kinds result_kind = foldr mkArrowKind result_kind arg_kinds
\end{code}


%************************************************************************
%*									*
\subsection{Wired-in type constructors
%*									*
%************************************************************************

We define a few wired-in type constructors here to avoid module knots

\begin{code}
funTyConName = mkWiredInTyConName funTyConKey pREL_GHC SLIT("(->)") funTyCon
funTyCon = mkFunTyCon funTyConName (mkArrowKinds [boxedTypeKind, boxedTypeKind] boxedTypeKind)
\end{code}


%************************************************************************
%*									*
\subsection{Equality on types}
%*									*
%************************************************************************

For the moment at least, type comparisons don't work if 
there are embedded for-alls.

\begin{code}
instance Eq Type where
  ty1 == ty2 = case ty1 `cmpTy` ty2 of { EQ -> True; other -> False }

instance Ord Type where
  compare ty1 ty2 = cmpTy ty1 ty2

cmpTy :: Type -> Type -> Ordering
cmpTy ty1 ty2
  = cmp emptyVarEnv ty1 ty2
  where
  -- The "env" maps type variables in ty1 to type variables in ty2
  -- So when comparing for-alls.. (forall tv1 . t1) (forall tv2 . t2)
  -- we in effect substitute tv2 for tv1 in t1 before continuing
    lookup env tv1 = case lookupVarEnv env tv1 of
			  Just tv2 -> tv2
			  Nothing  -> tv1

    -- Get rid of NoteTy
    cmp env (NoteTy _ ty1) ty2 = cmp env ty1 ty2
    cmp env ty1 (NoteTy _ ty2) = cmp env ty1 ty2
    
    -- Deal with equal constructors
    cmp env (TyVarTy tv1) (TyVarTy tv2) = lookup env tv1 `compare` tv2
    cmp env (AppTy f1 a1) (AppTy f2 a2) = cmp env f1 f2 `thenCmp` cmp env a1 a2
    cmp env (FunTy f1 a1) (FunTy f2 a2) = cmp env f1 f2 `thenCmp` cmp env a1 a2
    cmp env (TyConApp tc1 tys1) (TyConApp tc2 tys2) = (tc1 `compare` tc2) `thenCmp` (cmps env tys1 tys2)
    cmp env (ForAllTy tv1 t1)   (ForAllTy tv2 t2)   = cmp (extendVarEnv env tv1 tv2) t1 t2
    
    -- Deal with the rest: TyVarTy < AppTy < FunTy < TyConApp < ForAllTy
    cmp env (AppTy _ _) (TyVarTy _) = GT
    
    cmp env (FunTy _ _) (TyVarTy _) = GT
    cmp env (FunTy _ _) (AppTy _ _) = GT
    
    cmp env (TyConApp _ _) (TyVarTy _) = GT
    cmp env (TyConApp _ _) (AppTy _ _) = GT
    cmp env (TyConApp _ _) (FunTy _ _) = GT
    
    cmp env (ForAllTy _ _) other       = GT
    
    cmp env _ _		               = LT

    cmps env []     [] = EQ
    cmps env (t:ts) [] = GT
    cmps env [] (t:ts) = LT
    cmps env (t1:t1s) (t2:t2s) = cmp env t1 t2 `thenCmp` cmps env t1s t2s
\end{code}

