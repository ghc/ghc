%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%

Type - public interface

\begin{code}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-- | Main functions for manipulating types and type-related things
module Type (
	-- Note some of this is just re-exports from TyCon..

        -- * Main data types representing Types
	-- $type_classification
	
        -- $representation_types
	TyThing(..), Type, PredType(..), ThetaType,

        -- ** Constructing and deconstructing types
        mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe,

	mkAppTy, mkAppTys, splitAppTy, splitAppTys, 
	splitAppTy_maybe, repSplitAppTy_maybe,

	mkFunTy, mkFunTys, splitFunTy, splitFunTy_maybe, 
	splitFunTys, splitFunTysN,
	funResultTy, funArgTy, zipFunTys, 

	mkTyConApp, mkTyConTy, 
	tyConAppTyCon, tyConAppArgs, 
	splitTyConApp_maybe, splitTyConApp, 

        mkForAllTy, mkForAllTys, splitForAllTy_maybe, splitForAllTys, 
	applyTy, applyTys, applyTysD, isForAllTy, dropForAlls,
	
	-- (Newtypes)
	newTyConInstRhs, carefullySplitNewType_maybe,
	
	-- (Type families)
        tyFamInsts, predFamInsts,

        -- (Source types)
        mkPredTy, mkPredTys, mkFamilyTyConApp, isEqPred, coVarPred,

	-- ** Common type constructors
        funTyCon,

        -- ** Predicates on types
        isTyVarTy, isFunTy, isDictTy,

	-- (Lifting and boxity)
	isUnLiftedType, isUnboxedTupleType, isAlgType, isClosedAlgType,
	isPrimitiveType, isStrictType, isStrictPred, 

	-- * Main data types representing Kinds
	-- $kind_subtyping
        Kind, SimpleKind, KindVar,
        
        -- ** Common Kinds and SuperKinds
        liftedTypeKind, unliftedTypeKind, openTypeKind,
        argTypeKind, ubxTupleKind,

        tySuperKind, coSuperKind, 

        -- ** Common Kind type constructors
        liftedTypeKindTyCon, openTypeKindTyCon, unliftedTypeKindTyCon,
        argTypeKindTyCon, ubxTupleKindTyCon,

	-- * Type free variables
	tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tyVarsOfTheta,
	expandTypeSynonyms,

	-- * Type comparison
	coreEqType, coreEqType2,
        tcEqType, tcEqTypes, tcCmpType, tcCmpTypes, 
	tcEqPred, tcEqPredX, tcCmpPred, tcEqTypeX, tcPartOfType, tcPartOfPred,

	-- * Forcing evaluation of types
	seqType, seqTypes,

        -- * Other views onto Types
        coreView, tcView, kindView,

        repType, 

	-- * Type representation for the code generator
	PrimRep(..),

	typePrimRep, predTypeRep,

	-- * Main type substitution data types
	TvSubstEnv,	-- Representation widely visible
	TvSubst(..), 	-- Representation visible to a few friends
	
	-- ** Manipulating type substitutions
	emptyTvSubstEnv, emptyTvSubst,
	
	mkTvSubst, mkOpenTvSubst, zipOpenTvSubst, zipTopTvSubst, mkTopTvSubst, notElemTvSubst,
	getTvSubstEnv, setTvSubstEnv, zapTvSubstEnv, getTvInScope, 
        extendTvInScope, extendTvInScopeList,
 	extendTvSubst, extendTvSubstList, isInScope, composeTvSubst, zipTyEnv,
        isEmptyTvSubst,

	-- ** Performing substitution on types
	substTy, substTys, substTyWith, substTysWith, substTheta, 
	substPred, substTyVar, substTyVars, substTyVarBndr, deShadowTy, lookupTyVar,

	-- * Pretty-printing
	pprType, pprParendType, pprTypeApp, pprTyThingCategory, pprTyThing, pprForAll,
	pprPred, pprEqPred, pprTheta, pprThetaArrow, pprClassPred, pprKind, pprParendKind,
	
	pprSourceTyCon
    ) where

#include "HsVersions.h"

-- We import the representation and primitive functions from TypeRep.
-- Many things are reexported, but not the representation!

import TypeRep

-- friends:
import Var
import VarEnv
import VarSet

import Class
import TyCon

-- others
import StaticFlags
import Util
import Outputable
import FastString

import Data.Maybe	( isJust )

infixr 3 `mkFunTy`	-- Associates to the right
\end{code}

\begin{code}
-- $type_classification
-- #type_classification#
-- 
-- Types are one of:
-- 
-- [Unboxed]            Iff its representation is other than a pointer
-- 			Unboxed types are also unlifted.
-- 
-- [Lifted]             Iff it has bottom as an element.
-- 			Closures always have lifted types: i.e. any
-- 			let-bound identifier in Core must have a lifted
-- 			type. Operationally, a lifted object is one that
-- 			can be entered.
-- 			Only lifted types may be unified with a type variable.
-- 
-- [Algebraic]          Iff it is a type with one or more constructors, whether
-- 			declared with @data@ or @newtype@.
-- 			An algebraic type is one that can be deconstructed
-- 			with a case expression. This is /not/ the same as 
--			lifted types, because we also include unboxed
-- 			tuples in this classification.
-- 
-- [Data]               Iff it is a type declared with @data@, or a boxed tuple.
-- 
-- [Primitive]          Iff it is a built-in type that can't be expressed in Haskell.
-- 
-- Currently, all primitive types are unlifted, but that's not necessarily
-- the case: for example, @Int@ could be primitive.
-- 
-- Some primitive types are unboxed, such as @Int#@, whereas some are boxed
-- but unlifted (such as @ByteArray#@).  The only primitive types that we
-- classify as algebraic are the unboxed tuples.
-- 
-- Some examples of type classifications that may make this a bit clearer are:
-- 
-- @
-- Type         primitive       boxed           lifted          algebraic
-- -----------------------------------------------------------------------------
-- Int#         Yes             No              No              No
-- ByteArray#   Yes             Yes             No              No
-- (\# a, b \#)   Yes             No              No              Yes
-- (  a, b  )   No              Yes             Yes             Yes
-- [a]          No              Yes             Yes             Yes
-- @

-- $representation_types
-- A /source type/ is a type that is a separate type as far as the type checker is
-- concerned, but which has a more low-level representation as far as Core-to-Core
-- passes and the rest of the back end is concerned. Notably, 'PredTy's are removed
-- from the representation type while they do exist in the source types.
--
-- You don't normally have to worry about this, as the utility functions in
-- this module will automatically convert a source into a representation type
-- if they are spotted, to the best of it's abilities. If you don't want this
-- to happen, use the equivalent functions from the "TcType" module.
\end{code}

%************************************************************************
%*									*
		Type representation
%*									*
%************************************************************************

\begin{code}
{-# INLINE coreView #-}
coreView :: Type -> Maybe Type
-- ^ In Core, we \"look through\" non-recursive newtypes and 'PredTypes': this
-- function tries to obtain a different view of the supplied type given this
--
-- Strips off the /top layer only/ of a type to give 
-- its underlying representation type. 
-- Returns Nothing if there is nothing to look through.
--
-- In the case of @newtype@s, it returns one of:
--
-- 1) A vanilla 'TyConApp' (recursive newtype, or non-saturated)
-- 
-- 2) The newtype representation (otherwise), meaning the
--    type written in the RHS of the newtype declaration,
--    which may itself be a newtype
--
-- For example, with:
--
-- > newtype R = MkR S
-- > newtype S = MkS T
-- > newtype T = MkT (T -> T)
--
-- 'expandNewTcApp' on:
--
--  * @R@ gives @Just S@
--  * @S@ gives @Just T@
--  * @T@ gives @Nothing@ (no expansion)

-- By being non-recursive and inlined, this case analysis gets efficiently
-- joined onto the case analysis that the caller is already doing
coreView (PredTy p)
  | isEqPred p             = Nothing
  | otherwise    	   = Just (predTypeRep p)
coreView (TyConApp tc tys) | Just (tenv, rhs, tys') <- coreExpandTyCon_maybe tc tys 
			   = Just (mkAppTys (substTy (mkTopTvSubst tenv) rhs) tys')
				-- Its important to use mkAppTys, rather than (foldl AppTy),
				-- because the function part might well return a 
				-- partially-applied type constructor; indeed, usually will!
coreView _                 = Nothing



-----------------------------------------------
{-# INLINE tcView #-}
tcView :: Type -> Maybe Type
-- ^ Similar to 'coreView', but for the type checker, which just looks through synonyms
tcView (TyConApp tc tys) | Just (tenv, rhs, tys') <- tcExpandTyCon_maybe tc tys 
			 = Just (mkAppTys (substTy (mkTopTvSubst tenv) rhs) tys')
tcView _                 = Nothing

-----------------------------------------------
expandTypeSynonyms :: Type -> Type
-- ^ Expand out all type synonyms.  Actually, it'd suffice to expand out
-- just the ones that discard type variables (e.g.  type Funny a = Int)
-- But we don't know which those are currently, so we just expand all.
expandTypeSynonyms ty 
  = go ty
  where
    go (TyConApp tc tys)
      | Just (tenv, rhs, tys') <- tcExpandTyCon_maybe tc tys 
      = go (mkAppTys (substTy (mkTopTvSubst tenv) rhs) tys')
      | otherwise
      = TyConApp tc (map go tys)
    go (TyVarTy tv)    = TyVarTy tv
    go (AppTy t1 t2)   = AppTy (go t1) (go t2)
    go (FunTy t1 t2)   = FunTy (go t1) (go t2)
    go (ForAllTy tv t) = ForAllTy tv (go t)
    go (PredTy p)      = PredTy (go_pred p)

    go_pred (ClassP c ts)  = ClassP c (map go ts)
    go_pred (IParam ip t)  = IParam ip (go t)
    go_pred (EqPred t1 t2) = EqPred (go t1) (go t2)

-----------------------------------------------
{-# INLINE kindView #-}
kindView :: Kind -> Maybe Kind
-- ^ Similar to 'coreView' or 'tcView', but works on 'Kind's

-- For the moment, we don't even handle synonyms in kinds
kindView _            = Nothing
\end{code}


%************************************************************************
%*									*
\subsection{Constructor-specific functions}
%*									*
%************************************************************************


---------------------------------------------------------------------
				TyVarTy
				~~~~~~~
\begin{code}
mkTyVarTy  :: TyVar   -> Type
mkTyVarTy  = TyVarTy

mkTyVarTys :: [TyVar] -> [Type]
mkTyVarTys = map mkTyVarTy -- a common use of mkTyVarTy

-- | Attempts to obtain the type variable underlying a 'Type', and panics with the
-- given message if this is not a type variable type. See also 'getTyVar_maybe'
getTyVar :: String -> Type -> TyVar
getTyVar msg ty = case getTyVar_maybe ty of
		    Just tv -> tv
		    Nothing -> panic ("getTyVar: " ++ msg)

isTyVarTy :: Type -> Bool
isTyVarTy ty = isJust (getTyVar_maybe ty)

-- | Attempts to obtain the type variable underlying a 'Type'
getTyVar_maybe :: Type -> Maybe TyVar
getTyVar_maybe ty | Just ty' <- coreView ty = getTyVar_maybe ty'
getTyVar_maybe (TyVarTy tv) 	 	    = Just tv  
getTyVar_maybe _                            = Nothing

\end{code}


---------------------------------------------------------------------
				AppTy
				~~~~~
We need to be pretty careful with AppTy to make sure we obey the 
invariant that a TyConApp is always visibly so.  mkAppTy maintains the
invariant: use it.

\begin{code}
-- | Applies a type to another, as in e.g. @k a@
mkAppTy :: Type -> Type -> Type
mkAppTy orig_ty1 orig_ty2
  = mk_app orig_ty1
  where
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ [orig_ty2])
    mk_app _                 = AppTy orig_ty1 orig_ty2
	-- Note that the TyConApp could be an 
	-- under-saturated type synonym.  GHC allows that; e.g.
	--	type Foo k = k a -> k a
	--	type Id x = x
	--	foo :: Foo Id -> Foo Id
	--
	-- Here Id is partially applied in the type sig for Foo,
	-- but once the type synonyms are expanded all is well

mkAppTys :: Type -> [Type] -> Type
mkAppTys orig_ty1 []	    = orig_ty1
	-- This check for an empty list of type arguments
	-- avoids the needless loss of a type synonym constructor.
	-- For example: mkAppTys Rational []
	--   returns to (Ratio Integer), which has needlessly lost
	--   the Rational part.
mkAppTys orig_ty1 orig_tys2
  = mk_app orig_ty1
  where
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ orig_tys2)
				-- mkTyConApp: see notes with mkAppTy
    mk_app _                 = foldl AppTy orig_ty1 orig_tys2

-------------
splitAppTy_maybe :: Type -> Maybe (Type, Type)
-- ^ Attempt to take a type application apart, whether it is a
-- function, type constructor, or plain type application. Note
-- that type family applications are NEVER unsaturated by this!
splitAppTy_maybe ty | Just ty' <- coreView ty
		    = splitAppTy_maybe ty'
splitAppTy_maybe ty = repSplitAppTy_maybe ty

-------------
repSplitAppTy_maybe :: Type -> Maybe (Type,Type)
-- ^ Does the AppTy split as in 'splitAppTy_maybe', but assumes that 
-- any Core view stuff is already done
repSplitAppTy_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
repSplitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
repSplitAppTy_maybe (TyConApp tc tys) 
  | isDecomposableTyCon tc || length tys > tyConArity tc 
  = case snocView tys of       -- never create unsaturated type family apps
      Just (tys', ty') -> Just (TyConApp tc tys', ty')
      Nothing	       -> Nothing
repSplitAppTy_maybe _other = Nothing
-------------
splitAppTy :: Type -> (Type, Type)
-- ^ Attempts to take a type application apart, as in 'splitAppTy_maybe',
-- and panics if this is not possible
splitAppTy ty = case splitAppTy_maybe ty of
			Just pr -> pr
			Nothing -> panic "splitAppTy"

-------------
splitAppTys :: Type -> (Type, [Type])
-- ^ Recursively splits a type as far as is possible, leaving a residual
-- type being applied to and the type arguments applied to it. Never fails,
-- even if that means returning an empty list of type applications.
splitAppTys ty = split ty ty []
  where
    split orig_ty ty args | Just ty' <- coreView ty = split orig_ty ty' args
    split _       (AppTy ty arg)        args = split ty ty (arg:args)
    split _       (TyConApp tc tc_args) args
      = let -- keep type families saturated
            n | isDecomposableTyCon tc = 0
              | otherwise              = tyConArity tc
            (tc_args1, tc_args2) = splitAt n tc_args
        in
        (TyConApp tc tc_args1, tc_args2 ++ args)
    split _       (FunTy ty1 ty2)       args = ASSERT( null args )
					       (TyConApp funTyCon [], [ty1,ty2])
    split orig_ty _                     args = (orig_ty, args)

\end{code}


---------------------------------------------------------------------
				FunTy
				~~~~~

\begin{code}
mkFunTy :: Type -> Type -> Type
-- ^ Creates a function type from the given argument and result type
mkFunTy arg@(PredTy (EqPred {})) res = ForAllTy (mkWildCoVar arg) res
mkFunTy arg                      res = FunTy    arg               res

mkFunTys :: [Type] -> Type -> Type
mkFunTys tys ty = foldr mkFunTy ty tys

isFunTy :: Type -> Bool 
isFunTy ty = isJust (splitFunTy_maybe ty)

splitFunTy :: Type -> (Type, Type)
-- ^ Attempts to extract the argument and result types from a type, and
-- panics if that is not possible. See also 'splitFunTy_maybe'
splitFunTy ty | Just ty' <- coreView ty = splitFunTy ty'
splitFunTy (FunTy arg res)   = (arg, res)
splitFunTy other	     = pprPanic "splitFunTy" (ppr other)

splitFunTy_maybe :: Type -> Maybe (Type, Type)
-- ^ Attempts to extract the argument and result types from a type
splitFunTy_maybe ty | Just ty' <- coreView ty = splitFunTy_maybe ty'
splitFunTy_maybe (FunTy arg res)   = Just (arg, res)
splitFunTy_maybe _                 = Nothing

splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty ty | Just ty' <- coreView ty = split args orig_ty ty'
    split args _       (FunTy arg res)   = split (arg:args) res res
    split args orig_ty _                 = (reverse args, orig_ty)

splitFunTysN :: Int -> Type -> ([Type], Type)
-- ^ Split off exactly the given number argument types, and panics if that is not possible
splitFunTysN 0 ty = ([], ty)
splitFunTysN n ty = ASSERT2( isFunTy ty, int n <+> ppr ty )
                    case splitFunTy ty of { (arg, res) ->
		    case splitFunTysN (n-1) res of { (args, res) ->
		    (arg:args, res) }}

-- | Splits off argument types from the given type and associating
-- them with the things in the input list from left to right. The
-- final result type is returned, along with the resulting pairs of
-- objects and types, albeit with the list of pairs in reverse order.
-- Panics if there are not enough argument types for the input list.
zipFunTys :: Outputable a => [a] -> Type -> ([(a, Type)], Type)
zipFunTys orig_xs orig_ty = split [] orig_xs orig_ty orig_ty
  where
    split acc []     nty _                 = (reverse acc, nty)
    split acc xs     nty ty 
	  | Just ty' <- coreView ty 	   = split acc xs nty ty'
    split acc (x:xs) _   (FunTy arg res)   = split ((x,arg):acc) xs res res
    split _   _      _   _                 = pprPanic "zipFunTys" (ppr orig_xs <+> ppr orig_ty)
    
funResultTy :: Type -> Type
-- ^ Extract the function result type and panic if that is not possible
funResultTy ty | Just ty' <- coreView ty = funResultTy ty'
funResultTy (FunTy _arg res)  = res
funResultTy ty                = pprPanic "funResultTy" (ppr ty)

funArgTy :: Type -> Type
-- ^ Extract the function argument type and panic if that is not possible
funArgTy ty | Just ty' <- coreView ty = funArgTy ty'
funArgTy (FunTy arg _res)  = arg
funArgTy ty                = pprPanic "funArgTy" (ppr ty)
\end{code}

---------------------------------------------------------------------
				TyConApp
				~~~~~~~~

\begin{code}
-- | A key function: builds a 'TyConApp' or 'FunTy' as apppropriate to its arguments.
-- Applies its arguments to the constructor from left to right
mkTyConApp :: TyCon -> [Type] -> Type
mkTyConApp tycon tys
  | isFunTyCon tycon, [ty1,ty2] <- tys
  = FunTy ty1 ty2

  | otherwise
  = TyConApp tycon tys

-- | Create the plain type constructor type which has been applied to no type arguments at all.
mkTyConTy :: TyCon -> Type
mkTyConTy tycon = mkTyConApp tycon []

-- splitTyConApp "looks through" synonyms, because they don't
-- mean a distinct type, but all other type-constructor applications
-- including functions are returned as Just ..

-- | The same as @fst . splitTyConApp@
tyConAppTyCon :: Type -> TyCon
tyConAppTyCon ty = fst (splitTyConApp ty)

-- | The same as @snd . splitTyConApp@
tyConAppArgs :: Type -> [Type]
tyConAppArgs ty = snd (splitTyConApp ty)

-- | Attempts to tease a type apart into a type constructor and the application
-- of a number of arguments to that constructor. Panics if that is not possible.
-- See also 'splitTyConApp_maybe'
splitTyConApp :: Type -> (TyCon, [Type])
splitTyConApp ty = case splitTyConApp_maybe ty of
			Just stuff -> stuff
			Nothing	   -> pprPanic "splitTyConApp" (ppr ty)

-- | Attempts to tease a type apart into a type constructor and the application
-- of a number of arguments to that constructor
splitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
splitTyConApp_maybe ty | Just ty' <- coreView ty = splitTyConApp_maybe ty'
splitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
splitTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [arg,res])
splitTyConApp_maybe _                 = Nothing

newTyConInstRhs :: TyCon -> [Type] -> Type
-- ^ Unwrap one 'layer' of newtype on a type constructor and its arguments, using an 
-- eta-reduced version of the @newtype@ if possible
newTyConInstRhs tycon tys 
    = ASSERT2( equalLength tvs tys1, ppr tycon $$ ppr tys $$ ppr tvs )
      mkAppTys (substTyWith tvs tys1 ty) tys2
  where
    (tvs, ty)    = newTyConEtadRhs tycon
    (tys1, tys2) = splitAtList tvs tys
\end{code}


---------------------------------------------------------------------
				SynTy
				~~~~~

Notes on type synonyms
~~~~~~~~~~~~~~~~~~~~~~
The various "split" functions (splitFunTy, splitRhoTy, splitForAllTy) try
to return type synonyms whereever possible. Thus

	type Foo a = a -> a

we want 
	splitFunTys (a -> Foo a) = ([a], Foo a)
not			           ([a], a -> a)

The reason is that we then get better (shorter) type signatures in 
interfaces.  Notably this plays a role in tcTySigs in TcBinds.lhs.


Note [Expanding newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~
When expanding a type to expose a data-type constructor, we need to be
careful about newtypes, lest we fall into an infinite loop. Here are
the key examples:

  newtype Id  x = MkId x
  newtype Fix f = MkFix (f (Fix f))
  newtype T     = MkT (T -> T) 
  
  Type	         Expansion
 --------------------------
  T		 T -> T
  Fix Maybe      Maybe (Fix Maybe)
  Id (Id Int)    Int
  Fix Id         NO NO NO

Notice that we can expand T, even though it's recursive.
And we can expand Id (Id Int), even though the Id shows up
twice at the outer level.  

So, when expanding, we keep track of when we've seen a recursive
newtype at outermost level; and bale out if we see it again.


		Representation types
		~~~~~~~~~~~~~~~~~~~~

\begin{code}
-- | Looks through:
--
--	1. For-alls
--	2. Synonyms
--	3. Predicates
--	4. All newtypes, including recursive ones, but not newtype families
--
-- It's useful in the back end of the compiler.
repType :: Type -> Type
-- Only applied to types of kind *; hence tycons are saturated
repType ty
  = go [] ty
  where
    go :: [TyCon] -> Type -> Type
    go rec_nts ty | Just ty' <- coreView ty 	-- Expand synonyms
	= go rec_nts ty'	

    go rec_nts (ForAllTy _ ty)			-- Look through foralls
	= go rec_nts ty

    go rec_nts (TyConApp tc tys)		-- Expand newtypes
      | Just (rec_nts', ty') <- carefullySplitNewType_maybe rec_nts tc tys
      = go rec_nts' ty'

    go _ ty = ty


carefullySplitNewType_maybe :: [TyCon] -> TyCon -> [Type] -> Maybe ([TyCon],Type)
-- Return the representation of a newtype, unless 
-- we've seen it already: see Note [Expanding newtypes]
carefullySplitNewType_maybe rec_nts tc tys
  | isNewTyCon tc
  , not (tc `elem` rec_nts)  = Just (rec_nts', newTyConInstRhs tc tys)
  | otherwise	   	     = Nothing
  where
    rec_nts' | isRecursiveTyCon tc = tc:rec_nts
	     | otherwise	   = rec_nts


-- ToDo: this could be moved to the code generator, using splitTyConApp instead
-- of inspecting the type directly.

-- | Discovers the primitive representation of a more abstract 'Type'
typePrimRep :: Type -> PrimRep
typePrimRep ty = case repType ty of
		   TyConApp tc _ -> tyConPrimRep tc
		   FunTy _ _	 -> PtrRep
		   AppTy _ _	 -> PtrRep	-- See note below
		   TyVarTy _	 -> PtrRep
		   _             -> pprPanic "typePrimRep" (ppr ty)
	-- Types of the form 'f a' must be of kind *, not *#, so
	-- we are guaranteed that they are represented by pointers.
	-- The reason is that f must have kind *->*, not *->*#, because
	-- (we claim) there is no way to constrain f's kind any other
	-- way.
\end{code}


---------------------------------------------------------------------
				ForAllTy
				~~~~~~~~

\begin{code}
mkForAllTy :: TyVar -> Type -> Type
mkForAllTy tyvar ty
  = ForAllTy tyvar ty

-- | Wraps foralls over the type using the provided 'TyVar's from left to right
mkForAllTys :: [TyVar] -> Type -> Type
mkForAllTys tyvars ty = foldr ForAllTy ty tyvars

isForAllTy :: Type -> Bool
isForAllTy (ForAllTy _ _) = True
isForAllTy _              = False

-- | Attempts to take a forall type apart, returning the bound type variable
-- and the remainder of the type
splitForAllTy_maybe :: Type -> Maybe (TyVar, Type)
splitForAllTy_maybe ty = splitFAT_m ty
  where
    splitFAT_m ty | Just ty' <- coreView ty = splitFAT_m ty'
    splitFAT_m (ForAllTy tyvar ty)	    = Just(tyvar, ty)
    splitFAT_m _			    = Nothing

-- | Attempts to take a forall type apart, returning all the immediate such bound
-- type variables and the remainder of the type. Always suceeds, even if that means
-- returning an empty list of 'TyVar's
splitForAllTys :: Type -> ([TyVar], Type)
splitForAllTys ty = split ty ty []
   where
     split orig_ty ty tvs | Just ty' <- coreView ty = split orig_ty ty' tvs
     split _       (ForAllTy tv ty)  tvs = split ty ty (tv:tvs)
     split orig_ty _                 tvs = (reverse tvs, orig_ty)

-- | Equivalent to @snd . splitForAllTys@
dropForAlls :: Type -> Type
dropForAlls ty = snd (splitForAllTys ty)
\end{code}

-- (mkPiType now in CoreUtils)

applyTy, applyTys
~~~~~~~~~~~~~~~~~

\begin{code}
-- | Instantiate a forall type with one or more type arguments.
-- Used when we have a polymorphic function applied to type args:
--
-- > f t1 t2
--
-- We use @applyTys type-of-f [t1,t2]@ to compute the type of the expression.
-- Panics if no application is possible.
applyTy :: Type -> Type -> Type
applyTy ty arg | Just ty' <- coreView ty = applyTy ty' arg
applyTy (ForAllTy tv ty) arg = substTyWith [tv] [arg] ty
applyTy _                _   = panic "applyTy"

applyTys :: Type -> [Type] -> Type
-- ^ This function is interesting because:
--
--	1. The function may have more for-alls than there are args
--
--	2. Less obviously, it may have fewer for-alls
--
-- For case 2. think of:
--
-- > applyTys (forall a.a) [forall b.b, Int]
--
-- This really can happen, via dressing up polymorphic types with newtype
-- clothing.  Here's an example:
--
-- > newtype R = R (forall a. a->a)
-- > foo = case undefined :: R of
-- >            R f -> f ()

applyTys ty args = applyTysD empty ty args

applyTysD :: SDoc -> Type -> [Type] -> Type	-- Debug version
applyTysD _   orig_fun_ty []      = orig_fun_ty
applyTysD doc orig_fun_ty arg_tys 
  | n_tvs == n_args 	-- The vastly common case
  = substTyWith tvs arg_tys rho_ty
  | n_tvs > n_args 	-- Too many for-alls
  = substTyWith (take n_args tvs) arg_tys 
		(mkForAllTys (drop n_args tvs) rho_ty)
  | otherwise		-- Too many type args
  = ASSERT2( n_tvs > 0, doc $$ ppr orig_fun_ty )	-- Zero case gives infnite loop!
    applyTysD doc (substTyWith tvs (take n_tvs arg_tys) rho_ty)
	          (drop n_tvs arg_tys)
  where
    (tvs, rho_ty) = splitForAllTys orig_fun_ty 
    n_tvs = length tvs
    n_args = length arg_tys     
\end{code}


%************************************************************************
%*									*
\subsection{Source types}
%*									*
%************************************************************************

Source types are always lifted.

The key function is predTypeRep which gives the representation of a source type:

\begin{code}
mkPredTy :: PredType -> Type
mkPredTy pred = PredTy pred

mkPredTys :: ThetaType -> [Type]
mkPredTys preds = map PredTy preds

isEqPred :: PredType -> Bool
isEqPred (EqPred _ _) = True
isEqPred _            = False

predTypeRep :: PredType -> Type
-- ^ Convert a 'PredType' to its representation type. However, it unwraps 
-- only the outermost level; for example, the result might be a newtype application
predTypeRep (IParam _ ty)     = ty
predTypeRep (ClassP clas tys) = mkTyConApp (classTyCon clas) tys
	-- Result might be a newtype application, but the consumer will
	-- look through that too if necessary
predTypeRep (EqPred ty1 ty2) = pprPanic "predTypeRep" (ppr (EqPred ty1 ty2))

mkFamilyTyConApp :: TyCon -> [Type] -> Type
-- ^ Given a family instance TyCon and its arg types, return the
-- corresponding family type.  E.g:
--
-- > data family T a
-- > data instance T (Maybe b) = MkT b
--
-- Where the instance tycon is :RTL, so:
--
-- > mkFamilyTyConApp :RTL Int  =  T (Maybe Int)
mkFamilyTyConApp tc tys
  | Just (fam_tc, fam_tys) <- tyConFamInst_maybe tc
  , let fam_subst = zipTopTvSubst (tyConTyVars tc) tys
  = mkTyConApp fam_tc (substTys fam_subst fam_tys)
  | otherwise
  = mkTyConApp tc tys

-- | Pretty prints a 'TyCon', using the family instance in case of a
-- representation tycon.  For example:
--
-- > data T [a] = ...
--
-- In that case we want to print @T [a]@, where @T@ is the family 'TyCon'
pprSourceTyCon :: TyCon -> SDoc
pprSourceTyCon tycon 
  | Just (fam_tc, tys) <- tyConFamInst_maybe tycon
  = ppr $ fam_tc `TyConApp` tys	       -- can't be FunTyCon
  | otherwise
  = ppr tycon

isDictTy :: Type -> Bool
isDictTy ty = case splitTyConApp_maybe ty of
                Just (tc, _) -> isClassTyCon tc
		Nothing      -> False
\end{code}


%************************************************************************
%*									*
	     The free variables of a type
%*									*
%************************************************************************

\begin{code}
tyVarsOfType :: Type -> TyVarSet
-- ^ NB: for type synonyms tyVarsOfType does /not/ expand the synonym
tyVarsOfType (TyVarTy tv)     = unitVarSet tv
tyVarsOfType (TyConApp _ tys) = tyVarsOfTypes tys
tyVarsOfType (PredTy sty)     = tyVarsOfPred sty
tyVarsOfType (FunTy arg res)  = tyVarsOfType arg `unionVarSet` tyVarsOfType res
tyVarsOfType (AppTy fun arg)  = tyVarsOfType fun `unionVarSet` tyVarsOfType arg
tyVarsOfType (ForAllTy tv ty) -- The kind of a coercion binder 
	     	       	      -- can mention type variables!
  | isTyVar tv		      = inner_tvs `delVarSet` tv
  | otherwise  {- Coercion -} = -- ASSERT( not (tv `elemVarSet` inner_tvs) )
                                inner_tvs `unionVarSet` tyVarsOfType (tyVarKind tv)
  where
    inner_tvs = tyVarsOfType ty

tyVarsOfTypes :: [Type] -> TyVarSet
tyVarsOfTypes tys = foldr (unionVarSet.tyVarsOfType) emptyVarSet tys

tyVarsOfPred :: PredType -> TyVarSet
tyVarsOfPred (IParam _ ty)    = tyVarsOfType ty
tyVarsOfPred (ClassP _ tys)   = tyVarsOfTypes tys
tyVarsOfPred (EqPred ty1 ty2) = tyVarsOfType ty1 `unionVarSet` tyVarsOfType ty2

tyVarsOfTheta :: ThetaType -> TyVarSet
tyVarsOfTheta = foldr (unionVarSet . tyVarsOfPred) emptyVarSet
\end{code}


%************************************************************************
%*									*
\subsection{Type families}
%*									*
%************************************************************************

\begin{code}
-- | Finds type family instances occuring in a type after expanding synonyms.
tyFamInsts :: Type -> [(TyCon, [Type])]
tyFamInsts ty 
  | Just exp_ty <- tcView ty    = tyFamInsts exp_ty
tyFamInsts (TyVarTy _)          = []
tyFamInsts (TyConApp tc tys) 
  | isSynFamilyTyCon tc           = [(tc, tys)]
  | otherwise                   = concat (map tyFamInsts tys)
tyFamInsts (FunTy ty1 ty2)      = tyFamInsts ty1 ++ tyFamInsts ty2
tyFamInsts (AppTy ty1 ty2)      = tyFamInsts ty1 ++ tyFamInsts ty2
tyFamInsts (ForAllTy _ ty)      = tyFamInsts ty
tyFamInsts (PredTy pty)         = predFamInsts pty

-- | Finds type family instances occuring in a predicate type after expanding 
-- synonyms.
predFamInsts :: PredType -> [(TyCon, [Type])]
predFamInsts (ClassP _cla tys) = concat (map tyFamInsts tys)
predFamInsts (IParam _ ty)     = tyFamInsts ty
predFamInsts (EqPred ty1 ty2)  = tyFamInsts ty1 ++ tyFamInsts ty2
\end{code}


%************************************************************************
%*									*
\subsection{Liftedness}
%*									*
%************************************************************************

\begin{code}
-- | See "Type#type_classification" for what an unlifted type is
isUnLiftedType :: Type -> Bool
	-- isUnLiftedType returns True for forall'd unlifted types:
	--	x :: forall a. Int#
	-- I found bindings like these were getting floated to the top level.
	-- They are pretty bogus types, mind you.  It would be better never to
	-- construct them

isUnLiftedType ty | Just ty' <- coreView ty = isUnLiftedType ty'
isUnLiftedType (ForAllTy _ ty)   = isUnLiftedType ty
isUnLiftedType (TyConApp tc _)   = isUnLiftedTyCon tc
isUnLiftedType _                 = False

isUnboxedTupleType :: Type -> Bool
isUnboxedTupleType ty = case splitTyConApp_maybe ty of
                           Just (tc, _ty_args) -> isUnboxedTupleTyCon tc
                           _                   -> False

-- | See "Type#type_classification" for what an algebraic type is.
-- Should only be applied to /types/, as opposed to e.g. partially
-- saturated type constructors
isAlgType :: Type -> Bool
isAlgType ty 
  = case splitTyConApp_maybe ty of
      Just (tc, ty_args) -> ASSERT( ty_args `lengthIs` tyConArity tc )
			    isAlgTyCon tc
      _other	         -> False

-- | See "Type#type_classification" for what an algebraic type is.
-- Should only be applied to /types/, as opposed to e.g. partially
-- saturated type constructors. Closed type constructors are those
-- with a fixed right hand side, as opposed to e.g. associated types
isClosedAlgType :: Type -> Bool
isClosedAlgType ty
  = case splitTyConApp_maybe ty of
      Just (tc, ty_args) -> ASSERT( ty_args `lengthIs` tyConArity tc )
			    isAlgTyCon tc && not (isFamilyTyCon tc)
      _other	         -> False
\end{code}

\begin{code}
-- | Computes whether an argument (or let right hand side) should
-- be computed strictly or lazily, based only on its type.
-- Works just like 'isUnLiftedType', except that it has a special case 
-- for dictionaries (i.e. does not work purely on representation types)

-- Since it takes account of class 'PredType's, you might think
-- this function should be in 'TcType', but 'isStrictType' is used by 'DataCon',
-- which is below 'TcType' in the hierarchy, so it's convenient to put it here.
isStrictType :: Type -> Bool
isStrictType (PredTy pred)     = isStrictPred pred
isStrictType ty | Just ty' <- coreView ty = isStrictType ty'
isStrictType (ForAllTy _ ty)   = isStrictType ty
isStrictType (TyConApp tc _)   = isUnLiftedTyCon tc
isStrictType _                 = False

-- | We may be strict in dictionary types, but only if it 
-- has more than one component.
--
-- (Being strict in a single-component dictionary risks
--  poking the dictionary component, which is wrong.)
isStrictPred :: PredType -> Bool
isStrictPred (ClassP clas _) = opt_DictsStrict && not (isNewTyCon (classTyCon clas))
isStrictPred _               = False
\end{code}

\begin{code}
isPrimitiveType :: Type -> Bool
-- ^ Returns true of types that are opaque to Haskell.
-- Most of these are unlifted, but now that we interact with .NET, we
-- may have primtive (foreign-imported) types that are lifted
isPrimitiveType ty = case splitTyConApp_maybe ty of
			Just (tc, ty_args) -> ASSERT( ty_args `lengthIs` tyConArity tc )
					      isPrimTyCon tc
			_                  -> False
\end{code}


%************************************************************************
%*									*
\subsection{Sequencing on types}
%*									*
%************************************************************************

\begin{code}
seqType :: Type -> ()
seqType (TyVarTy tv) 	  = tv `seq` ()
seqType (AppTy t1 t2) 	  = seqType t1 `seq` seqType t2
seqType (FunTy t1 t2) 	  = seqType t1 `seq` seqType t2
seqType (PredTy p) 	  = seqPred p
seqType (TyConApp tc tys) = tc `seq` seqTypes tys
seqType (ForAllTy tv ty)  = tv `seq` seqType ty

seqTypes :: [Type] -> ()
seqTypes []       = ()
seqTypes (ty:tys) = seqType ty `seq` seqTypes tys

seqPred :: PredType -> ()
seqPred (ClassP c tys)   = c `seq` seqTypes tys
seqPred (IParam n ty)    = n `seq` seqType ty
seqPred (EqPred ty1 ty2) = seqType ty1 `seq` seqType ty2
\end{code}


%************************************************************************
%*									*
		Equality for Core types 
	(We don't use instances so that we know where it happens)
%*									*
%************************************************************************

Note that eqType works right even for partial applications of newtypes.
See Note [Newtype eta] in TyCon.lhs

\begin{code}
-- | Type equality test for Core types (i.e. ignores predicate-types, synonyms etc.)
coreEqType :: Type -> Type -> Bool
coreEqType t1 t2 = coreEqType2 rn_env t1 t2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyVarsOfType t1 `unionVarSet` tyVarsOfType t2))

coreEqType2 :: RnEnv2 -> Type -> Type -> Bool
coreEqType2 rn_env t1 t2
  = eq rn_env t1 t2
  where
    eq env (TyVarTy tv1)       (TyVarTy tv2)     = rnOccL env tv1 == rnOccR env tv2
    eq env (ForAllTy tv1 t1)   (ForAllTy tv2 t2) = eq (rnBndr2 env tv1 tv2) t1 t2
    eq env (AppTy s1 t1)       (AppTy s2 t2)     = eq env s1 s2 && eq env t1 t2
    eq env (FunTy s1 t1)       (FunTy s2 t2)     = eq env s1 s2 && eq env t1 t2
    eq env (TyConApp tc1 tys1) (TyConApp tc2 tys2) 
	| tc1 == tc2, all2 (eq env) tys1 tys2 = True
			-- The lengths should be equal because
			-- the two types have the same kind
	-- NB: if the type constructors differ that does not 
	--     necessarily mean that the types aren't equal
	--     (synonyms, newtypes)
	-- Even if the type constructors are the same, but the arguments
	-- differ, the two types could be the same (e.g. if the arg is just
	-- ignored in the RHS).  In both these cases we fall through to an 
	-- attempt to expand one side or the other.

	-- Now deal with newtypes, synonyms, pred-tys
    eq env t1 t2 | Just t1' <- coreView t1 = eq env t1' t2 
		 | Just t2' <- coreView t2 = eq env t1 t2' 

	-- Fall through case; not equal!
    eq _ _ _ = False
\end{code}


%************************************************************************
%*									*
		Comparision for source types 
	(We don't use instances so that we know where it happens)
%*									*
%************************************************************************

\begin{code}
tcEqType :: Type -> Type -> Bool
-- ^ Type equality on source types. Does not look through @newtypes@ or 
-- 'PredType's, but it does look through type synonyms.
tcEqType t1 t2 = isEqual $ cmpType t1 t2

tcEqTypes :: [Type] -> [Type] -> Bool
tcEqTypes tys1 tys2 = isEqual $ cmpTypes tys1 tys2

tcCmpType :: Type -> Type -> Ordering
-- ^ Type ordering on source types. Does not look through @newtypes@ or 
-- 'PredType's, but it does look through type synonyms.
tcCmpType t1 t2 = cmpType t1 t2

tcCmpTypes :: [Type] -> [Type] -> Ordering
tcCmpTypes tys1 tys2 = cmpTypes tys1 tys2

tcEqPred :: PredType -> PredType -> Bool
tcEqPred p1 p2 = isEqual $ cmpPred p1 p2

tcEqPredX :: RnEnv2 -> PredType -> PredType -> Bool
tcEqPredX env p1 p2 = isEqual $ cmpPredX env p1 p2

tcCmpPred :: PredType -> PredType -> Ordering
tcCmpPred p1 p2 = cmpPred p1 p2

tcEqTypeX :: RnEnv2 -> Type -> Type -> Bool
tcEqTypeX env t1 t2 = isEqual $ cmpTypeX env t1 t2
\end{code}

\begin{code}
-- | Checks whether the second argument is a subterm of the first.  (We don't care
-- about binders, as we are only interested in syntactic subterms.)
tcPartOfType :: Type -> Type -> Bool
tcPartOfType t1              t2 
  | tcEqType t1 t2              = True
tcPartOfType t1              t2 
  | Just t2' <- tcView t2       = tcPartOfType t1 t2'
tcPartOfType _  (TyVarTy _)     = False
tcPartOfType t1 (ForAllTy _ t2) = tcPartOfType t1 t2
tcPartOfType t1 (AppTy s2 t2)   = tcPartOfType t1 s2 || tcPartOfType t1 t2
tcPartOfType t1 (FunTy s2 t2)   = tcPartOfType t1 s2 || tcPartOfType t1 t2
tcPartOfType t1 (PredTy p2)     = tcPartOfPred t1 p2
tcPartOfType t1 (TyConApp _ ts) = any (tcPartOfType t1) ts

tcPartOfPred :: Type -> PredType -> Bool
tcPartOfPred t1 (IParam _ t2)  = tcPartOfType t1 t2
tcPartOfPred t1 (ClassP _ ts)  = any (tcPartOfType t1) ts
tcPartOfPred t1 (EqPred s2 t2) = tcPartOfType t1 s2 || tcPartOfType t1 t2
\end{code}

Now here comes the real worker

\begin{code}
cmpType :: Type -> Type -> Ordering
cmpType t1 t2 = cmpTypeX rn_env t1 t2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyVarsOfType t1 `unionVarSet` tyVarsOfType t2))

cmpTypes :: [Type] -> [Type] -> Ordering
cmpTypes ts1 ts2 = cmpTypesX rn_env ts1 ts2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyVarsOfTypes ts1 `unionVarSet` tyVarsOfTypes ts2))

cmpPred :: PredType -> PredType -> Ordering
cmpPred p1 p2 = cmpPredX rn_env p1 p2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyVarsOfPred p1 `unionVarSet` tyVarsOfPred p2))

cmpTypeX :: RnEnv2 -> Type -> Type -> Ordering	-- Main workhorse
cmpTypeX env t1 t2 | Just t1' <- tcView t1 = cmpTypeX env t1' t2
		   | Just t2' <- tcView t2 = cmpTypeX env t1 t2'

cmpTypeX env (TyVarTy tv1)       (TyVarTy tv2)       = rnOccL env tv1 `compare` rnOccR env tv2
cmpTypeX env (ForAllTy tv1 t1)   (ForAllTy tv2 t2)   = cmpTypeX (rnBndr2 env tv1 tv2) t1 t2
cmpTypeX env (AppTy s1 t1)       (AppTy s2 t2)       = cmpTypeX env s1 s2 `thenCmp` cmpTypeX env t1 t2
cmpTypeX env (FunTy s1 t1)       (FunTy s2 t2)       = cmpTypeX env s1 s2 `thenCmp` cmpTypeX env t1 t2
cmpTypeX env (PredTy p1)         (PredTy p2)         = cmpPredX env p1 p2
cmpTypeX env (TyConApp tc1 tys1) (TyConApp tc2 tys2) = (tc1 `compare` tc2) `thenCmp` cmpTypesX env tys1 tys2

    -- Deal with the rest: TyVarTy < AppTy < FunTy < TyConApp < ForAllTy < PredTy
cmpTypeX _ (AppTy _ _)    (TyVarTy _)    = GT

cmpTypeX _ (FunTy _ _)    (TyVarTy _)    = GT
cmpTypeX _ (FunTy _ _)    (AppTy _ _)    = GT

cmpTypeX _ (TyConApp _ _) (TyVarTy _)    = GT
cmpTypeX _ (TyConApp _ _) (AppTy _ _)    = GT
cmpTypeX _ (TyConApp _ _) (FunTy _ _)    = GT

cmpTypeX _ (ForAllTy _ _) (TyVarTy _)    = GT
cmpTypeX _ (ForAllTy _ _) (AppTy _ _)    = GT
cmpTypeX _ (ForAllTy _ _) (FunTy _ _)    = GT
cmpTypeX _ (ForAllTy _ _) (TyConApp _ _) = GT

cmpTypeX _ (PredTy _)     _              = GT

cmpTypeX _ _              _              = LT

-------------
cmpTypesX :: RnEnv2 -> [Type] -> [Type] -> Ordering
cmpTypesX _   []        []        = EQ
cmpTypesX env (t1:tys1) (t2:tys2) = cmpTypeX env t1 t2 `thenCmp` cmpTypesX env tys1 tys2
cmpTypesX _   []        _         = LT
cmpTypesX _   _         []        = GT

-------------
cmpPredX :: RnEnv2 -> PredType -> PredType -> Ordering
cmpPredX env (IParam n1 ty1) (IParam n2 ty2) = (n1 `compare` n2) `thenCmp` cmpTypeX env ty1 ty2
	-- Compare names only for implicit parameters
	-- This comparison is used exclusively (I believe) 
	-- for the Avails finite map built in TcSimplify
	-- If the types differ we keep them distinct so that we see 
	-- a distinct pair to run improvement on 
cmpPredX env (ClassP c1 tys1) (ClassP c2 tys2) = (c1 `compare` c2) `thenCmp` (cmpTypesX env tys1 tys2)
cmpPredX env (EqPred ty1 ty2) (EqPred ty1' ty2') = (cmpTypeX env ty1 ty1') `thenCmp` (cmpTypeX env ty2 ty2')

-- Constructor order: IParam < ClassP < EqPred
cmpPredX _   (IParam {})     _              = LT
cmpPredX _   (ClassP {})    (IParam {})     = GT
cmpPredX _   (ClassP {})    (EqPred {})     = LT
cmpPredX _   (EqPred {})    _               = GT
\end{code}

PredTypes are used as a FM key in TcSimplify, 
so we take the easy path and make them an instance of Ord

\begin{code}
instance Eq  PredType where { (==)    = tcEqPred }
instance Ord PredType where { compare = tcCmpPred }
\end{code}


%************************************************************************
%*									*
		Type substitutions
%*									*
%************************************************************************

\begin{code}
-- | Type substitution
--
-- #tvsubst_invariant#
-- The following invariants must hold of a 'TvSubst':
-- 
-- 1. The in-scope set is needed /only/ to
-- guide the generation of fresh uniques
--
-- 2. In particular, the /kind/ of the type variables in 
-- the in-scope set is not relevant
--
-- 3. The substition is only applied ONCE! This is because
-- in general such application will not reached a fixed point.
data TvSubst 		
  = TvSubst InScopeSet 	-- The in-scope type variables
	    TvSubstEnv	-- The substitution itself
	-- See Note [Apply Once]
	-- and Note [Extending the TvSubstEnv]

{- ----------------------------------------------------------

Note [Apply Once]
~~~~~~~~~~~~~~~~~
We use TvSubsts to instantiate things, and we might instantiate
	forall a b. ty
\with the types
	[a, b], or [b, a].
So the substition might go [a->b, b->a].  A similar situation arises in Core
when we find a beta redex like
	(/\ a /\ b -> e) b a
Then we also end up with a substition that permutes type variables. Other
variations happen to; for example [a -> (a, b)].  

	***************************************************
	*** So a TvSubst must be applied precisely once ***
	***************************************************

A TvSubst is not idempotent, but, unlike the non-idempotent substitution
we use during unifications, it must not be repeatedly applied.

Note [Extending the TvSubst]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #tvsubst_invariant# for the invariants that must hold.

This invariant allows a short-cut when the TvSubstEnv is empty:
if the TvSubstEnv is empty --- i.e. (isEmptyTvSubt subst) holds ---
then (substTy subst ty) does nothing.

For example, consider:
	(/\a. /\b:(a~Int). ...b..) Int
We substitute Int for 'a'.  The Unique of 'b' does not change, but
nevertheless we add 'b' to the TvSubstEnv, because b's kind does change

This invariant has several crucial consequences:

* In substTyVarBndr, we need extend the TvSubstEnv 
	- if the unique has changed
	- or if the kind has changed

* In substTyVar, we do not need to consult the in-scope set;
  the TvSubstEnv is enough

* In substTy, substTheta, we can short-circuit when the TvSubstEnv is empty
  

-------------------------------------------------------------- -}

-- | A substitition of 'Type's for 'TyVar's
type TvSubstEnv = TyVarEnv Type
	-- A TvSubstEnv is used both inside a TvSubst (with the apply-once
	-- invariant discussed in Note [Apply Once]), and also independently
	-- in the middle of matching, and unification (see Types.Unify)
	-- So you have to look at the context to know if it's idempotent or
	-- apply-once or whatever

emptyTvSubstEnv :: TvSubstEnv
emptyTvSubstEnv = emptyVarEnv

composeTvSubst :: InScopeSet -> TvSubstEnv -> TvSubstEnv -> TvSubstEnv
-- ^ @(compose env1 env2)(x)@ is @env1(env2(x))@; i.e. apply @env2@ then @env1@.
-- It assumes that both are idempotent.
-- Typically, @env1@ is the refinement to a base substitution @env2@
composeTvSubst in_scope env1 env2
  = env1 `plusVarEnv` mapVarEnv (substTy subst1) env2
	-- First apply env1 to the range of env2
	-- Then combine the two, making sure that env1 loses if
	-- both bind the same variable; that's why env1 is the
	--  *left* argument to plusVarEnv, because the right arg wins
  where
    subst1 = TvSubst in_scope env1

emptyTvSubst :: TvSubst
emptyTvSubst = TvSubst emptyInScopeSet emptyVarEnv

isEmptyTvSubst :: TvSubst -> Bool
	 -- See Note [Extending the TvSubstEnv]
isEmptyTvSubst (TvSubst _ env) = isEmptyVarEnv env

mkTvSubst :: InScopeSet -> TvSubstEnv -> TvSubst
mkTvSubst = TvSubst

getTvSubstEnv :: TvSubst -> TvSubstEnv
getTvSubstEnv (TvSubst _ env) = env

getTvInScope :: TvSubst -> InScopeSet
getTvInScope (TvSubst in_scope _) = in_scope

isInScope :: Var -> TvSubst -> Bool
isInScope v (TvSubst in_scope _) = v `elemInScopeSet` in_scope

notElemTvSubst :: TyVar -> TvSubst -> Bool
notElemTvSubst tv (TvSubst _ env) = not (tv `elemVarEnv` env)

setTvSubstEnv :: TvSubst -> TvSubstEnv -> TvSubst
setTvSubstEnv (TvSubst in_scope _) env = TvSubst in_scope env

zapTvSubstEnv :: TvSubst -> TvSubst
zapTvSubstEnv (TvSubst in_scope _) = TvSubst in_scope emptyVarEnv

extendTvInScope :: TvSubst -> Var -> TvSubst
extendTvInScope (TvSubst in_scope env) var = TvSubst (extendInScopeSet in_scope var) env

extendTvInScopeList :: TvSubst -> [Var] -> TvSubst
extendTvInScopeList (TvSubst in_scope env) vars = TvSubst (extendInScopeSetList in_scope vars) env

extendTvSubst :: TvSubst -> TyVar -> Type -> TvSubst
extendTvSubst (TvSubst in_scope env) tv ty = TvSubst in_scope (extendVarEnv env tv ty)

extendTvSubstList :: TvSubst -> [TyVar] -> [Type] -> TvSubst
extendTvSubstList (TvSubst in_scope env) tvs tys 
  = TvSubst in_scope (extendVarEnvList env (tvs `zip` tys))

-- mkOpenTvSubst and zipOpenTvSubst generate the in-scope set from
-- the types given; but it's just a thunk so with a bit of luck
-- it'll never be evaluated

-- Note [Generating the in-scope set for a substitution]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If we want to substitute [a -> ty1, b -> ty2] I used to 
-- think it was enough to generate an in-scope set that includes
-- fv(ty1,ty2).  But that's not enough; we really should also take the
-- free vars of the type we are substituting into!  Example:
--	(forall b. (a,b,x)) [a -> List b]
-- Then if we use the in-scope set {b}, there is a danger we will rename
-- the forall'd variable to 'x' by mistake, getting this:
--	(forall x. (List b, x, x)
-- Urk!  This means looking at all the calls to mkOpenTvSubst....


-- | Generates the in-scope set for the 'TvSubst' from the types in the incoming
-- environment, hence "open"
mkOpenTvSubst :: TvSubstEnv -> TvSubst
mkOpenTvSubst env = TvSubst (mkInScopeSet (tyVarsOfTypes (varEnvElts env))) env

-- | Generates the in-scope set for the 'TvSubst' from the types in the incoming
-- environment, hence "open"
zipOpenTvSubst :: [TyVar] -> [Type] -> TvSubst
zipOpenTvSubst tyvars tys 
  | debugIsOn && (length tyvars /= length tys)
  = pprTrace "zipOpenTvSubst" (ppr tyvars $$ ppr tys) emptyTvSubst
  | otherwise
  = TvSubst (mkInScopeSet (tyVarsOfTypes tys)) (zipTyEnv tyvars tys)

-- | Called when doing top-level substitutions. Here we expect that the 
-- free vars of the range of the substitution will be empty.
mkTopTvSubst :: [(TyVar, Type)] -> TvSubst
mkTopTvSubst prs = TvSubst emptyInScopeSet (mkVarEnv prs)

zipTopTvSubst :: [TyVar] -> [Type] -> TvSubst
zipTopTvSubst tyvars tys 
  | debugIsOn && (length tyvars /= length tys)
  = pprTrace "zipTopTvSubst" (ppr tyvars $$ ppr tys) emptyTvSubst
  | otherwise
  = TvSubst emptyInScopeSet (zipTyEnv tyvars tys)

zipTyEnv :: [TyVar] -> [Type] -> TvSubstEnv
zipTyEnv tyvars tys
  | debugIsOn && (length tyvars /= length tys)
  = pprTrace "mkTopTvSubst" (ppr tyvars $$ ppr tys) emptyVarEnv
  | otherwise
  = zip_ty_env tyvars tys emptyVarEnv

-- Later substitutions in the list over-ride earlier ones, 
-- but there should be no loops
zip_ty_env :: [TyVar] -> [Type] -> TvSubstEnv -> TvSubstEnv
zip_ty_env []       []       env = env
zip_ty_env (tv:tvs) (ty:tys) env = zip_ty_env tvs tys (extendVarEnv env tv ty)
	-- There used to be a special case for when 
	--	ty == TyVarTy tv
	-- (a not-uncommon case) in which case the substitution was dropped.
	-- But the type-tidier changes the print-name of a type variable without
	-- changing the unique, and that led to a bug.   Why?  Pre-tidying, we had 
	-- a type {Foo t}, where Foo is a one-method class.  So Foo is really a newtype.
	-- And it happened that t was the type variable of the class.  Post-tiding, 
	-- it got turned into {Foo t2}.  The ext-core printer expanded this using
	-- sourceTypeRep, but that said "Oh, t == t2" because they have the same unique,
	-- and so generated a rep type mentioning t not t2.  
	--
	-- Simplest fix is to nuke the "optimisation"
zip_ty_env tvs      tys      env   = pprTrace "Var/Type length mismatch: " (ppr tvs $$ ppr tys) env
-- zip_ty_env _ _ env = env

instance Outputable TvSubst where
  ppr (TvSubst ins env) 
    = brackets $ sep[ ptext (sLit "TvSubst"),
		      nest 2 (ptext (sLit "In scope:") <+> ppr ins), 
		      nest 2 (ptext (sLit "Env:") <+> ppr env) ]
\end{code}

%************************************************************************
%*									*
		Performing type substitutions
%*									*
%************************************************************************

\begin{code}
-- | Type substitution making use of an 'TvSubst' that
-- is assumed to be open, see 'zipOpenTvSubst'
substTyWith :: [TyVar] -> [Type] -> Type -> Type
substTyWith tvs tys = ASSERT( length tvs == length tys )
		      substTy (zipOpenTvSubst tvs tys)

-- | Type substitution making use of an 'TvSubst' that
-- is assumed to be open, see 'zipOpenTvSubst'
substTysWith :: [TyVar] -> [Type] -> [Type] -> [Type]
substTysWith tvs tys = ASSERT( length tvs == length tys )
		       substTys (zipOpenTvSubst tvs tys)

-- | Substitute within a 'Type'
substTy :: TvSubst -> Type  -> Type
substTy subst ty | isEmptyTvSubst subst = ty
		 | otherwise	        = subst_ty subst ty

-- | Substitute within several 'Type's
substTys :: TvSubst -> [Type] -> [Type]
substTys subst tys | isEmptyTvSubst subst = tys
	           | otherwise	          = map (subst_ty subst) tys

-- | Substitute within a 'ThetaType'
substTheta :: TvSubst -> ThetaType -> ThetaType
substTheta subst theta
  | isEmptyTvSubst subst = theta
  | otherwise	         = map (substPred subst) theta

-- | Substitute within a 'PredType'
substPred :: TvSubst -> PredType -> PredType
substPred subst (IParam n ty)     = IParam n (subst_ty subst ty)
substPred subst (ClassP clas tys) = ClassP clas (map (subst_ty subst) tys)
substPred subst (EqPred ty1 ty2)  = EqPred (subst_ty subst ty1) (subst_ty subst ty2)

-- | Remove any nested binders mentioning the 'TyVar's in the 'TyVarSet'
deShadowTy :: TyVarSet -> Type -> Type
deShadowTy tvs ty 
  = subst_ty (mkTvSubst in_scope emptyTvSubstEnv) ty
  where
    in_scope = mkInScopeSet tvs

subst_ty :: TvSubst -> Type -> Type
-- subst_ty is the main workhorse for type substitution
--
-- Note that the in_scope set is poked only if we hit a forall
-- so it may often never be fully computed 
subst_ty subst ty
   = go ty
  where
    go (TyVarTy tv)      = substTyVar subst tv
    go (TyConApp tc tys) = let args = map go tys
                           in  args `seqList` TyConApp tc args

    go (PredTy p)        = PredTy $! (substPred subst p)

    go (FunTy arg res)   = (FunTy $! (go arg)) $! (go res)
    go (AppTy fun arg)   = mkAppTy (go fun) $! (go arg)
                -- The mkAppTy smart constructor is important
                -- we might be replacing (a Int), represented with App
                -- by [Int], represented with TyConApp
    go (ForAllTy tv ty)  = case substTyVarBndr subst tv of
                              (subst', tv') ->
                                 ForAllTy tv' $! (subst_ty subst' ty)

substTyVar :: TvSubst -> TyVar  -> Type
substTyVar subst@(TvSubst _ _) tv
  = case lookupTyVar subst tv of {
	Nothing -> TyVarTy tv;
       	Just ty -> ty	-- See Note [Apply Once]
    } 

substTyVars :: TvSubst -> [TyVar] -> [Type]
substTyVars subst tvs = map (substTyVar subst) tvs

lookupTyVar :: TvSubst -> TyVar  -> Maybe Type
	-- See Note [Extending the TvSubst]
lookupTyVar (TvSubst _ env) tv = lookupVarEnv env tv

substTyVarBndr :: TvSubst -> TyVar -> (TvSubst, TyVar)	
substTyVarBndr subst@(TvSubst in_scope env) old_var
  = (TvSubst (in_scope `extendInScopeSet` new_var) new_env, new_var)
  where
    is_co_var = isCoVar old_var

    new_env | no_change = delVarEnv env old_var
	    | otherwise = extendVarEnv env old_var (TyVarTy new_var)

    no_change = new_var == old_var && not is_co_var
	-- no_change means that the new_var is identical in
	-- all respects to the old_var (same unique, same kind)
	-- See Note [Extending the TvSubst]
	--
	-- In that case we don't need to extend the substitution
	-- to map old to new.  But instead we must zap any 
	-- current substitution for the variable. For example:
	--	(\x.e) with id_subst = [x |-> e']
	-- Here we must simply zap the substitution for x

    new_var = uniqAway in_scope subst_old_var
	-- The uniqAway part makes sure the new variable is not already in scope

    subst_old_var -- subst_old_var is old_var with the substitution applied to its kind
		  -- It's only worth doing the substitution for coercions,
		  -- becuase only they can have free type variables
	| is_co_var = setTyVarKind old_var (substTy subst (tyVarKind old_var))
	| otherwise = old_var
\end{code}

----------------------------------------------------
-- Kind Stuff

Kinds
~~~~~

\begin{code}
-- $kind_subtyping
-- #kind_subtyping#
-- There's a little subtyping at the kind level:
--
-- @
--               ?
--              \/ &#92;
--             \/   &#92;
--            ??   (\#)
--           \/  &#92;
--          \*    \#
-- .
-- Where:        \*    [LiftedTypeKind]   means boxed type
--              \#    [UnliftedTypeKind] means unboxed type
--              (\#)  [UbxTupleKind]     means unboxed tuple
--              ??   [ArgTypeKind]      is the lub of {\*, \#}
--              ?    [OpenTypeKind]	means any type at all
-- @
--
-- In particular:
--
-- > error :: forall a:?. String -> a
-- > (->)  :: ?? -> ? -> \*
-- > (\\(x::t) -> ...)
--
-- Where in the last example @t :: ??@ (i.e. is not an unboxed tuple)

type KindVar = TyVar  -- invariant: KindVar will always be a 
                      -- TcTyVar with details MetaTv TauTv ...
-- kind var constructors and functions are in TcType

type SimpleKind = Kind
\end{code}

Kind inference
~~~~~~~~~~~~~~
During kind inference, a kind variable unifies only with 
a "simple kind", sk
	sk ::= * | sk1 -> sk2
For example 
	data T a = MkT a (T Int#)
fails.  We give T the kind (k -> *), and the kind variable k won't unify
with # (the kind of Int#).

Type inference
~~~~~~~~~~~~~~
When creating a fresh internal type variable, we give it a kind to express 
constraints on it.  E.g. in (\x->e) we make up a fresh type variable for x, 
with kind ??.  

During unification we only bind an internal type variable to a type
whose kind is lower in the sub-kind hierarchy than the kind of the tyvar.

When unifying two internal type variables, we collect their kind constraints by
finding the GLB of the two.  Since the partial order is a tree, they only
have a glb if one is a sub-kind of the other.  In that case, we bind the
less-informative one to the more informative one.  Neat, eh?
