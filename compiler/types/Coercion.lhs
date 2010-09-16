%
% (c) The University of Glasgow 2006
%

\begin{code}
-- | Module for (a) type kinds and (b) type coercions, 
-- as used in System FC. See 'CoreSyn.Expr' for
-- more on System FC and how coercions fit into it.
--
-- Coercions are represented as types, and their kinds tell what types the 
-- coercion works on. The coercion kind constructor is a special TyCon that 
-- must always be saturated, like so:
--
-- > typeKind (symCoercion type) :: TyConApp CoTyCon{...} [type, type]
module Coercion (
        -- * Main data type
        Coercion, Kind,
        typeKind,

        -- ** Deconstructing Kinds 
        kindFunResult, kindAppResult, synTyConResKind,
        splitKindFunTys, splitKindFunTysN, splitKindFunTy_maybe,

        -- ** Predicates on Kinds
        isLiftedTypeKind, isUnliftedTypeKind, isOpenTypeKind,
        isUbxTupleKind, isArgTypeKind, isKind, isTySuperKind, 
        isCoSuperKind, isSuperKind, isCoercionKind, 
	mkArrowKind, mkArrowKinds,

        isSubArgTypeKind, isSubOpenTypeKind, isSubKind, defaultKind, eqKind,
        isSubKindCon,

        mkCoKind, mkCoPredTy, coVarKind, coVarKind_maybe,
        coercionKind, coercionKinds, isIdentityCoercion,

	-- ** Equality predicates
	isEqPred, mkEqPred, getEqPredTys, isEqPredTy,  

	-- ** Coercion transformations
	mkCoercion,
        mkSymCoercion, mkTransCoercion,
        mkLeftCoercion, mkRightCoercion, 
	mkInstCoercion, mkAppCoercion, mkTyConCoercion, mkFunCoercion,
        mkForAllCoercion, mkInstsCoercion, mkUnsafeCoercion,
        mkNewTypeCoercion, mkFamInstCoercion, mkAppsCoercion,
        mkCsel1Coercion, mkCsel2Coercion, mkCselRCoercion, 
                       
        mkCoVarCoercion, 


        unsafeCoercionTyCon, symCoercionTyCon,
        transCoercionTyCon, leftCoercionTyCon, 
        rightCoercionTyCon, instCoercionTyCon, -- needed by TysWiredIn
        csel1CoercionTyCon, csel2CoercionTyCon, cselRCoercionTyCon, 

        -- ** Decomposition
        decompLR_maybe, decompCsel_maybe, decompInst_maybe,
        splitCoPredTy_maybe,
        splitNewTypeRepCo_maybe, instNewTyCon_maybe, decomposeCo,

        -- ** Comparison
        coreEqCoercion, coreEqCoercion2,

	-- * CoercionI
	CoercionI(..),
	isIdentityCoI,
	mkSymCoI, mkTransCoI, 
	mkTyConAppCoI, mkAppTyCoI, mkFunTyCoI,
	mkForAllTyCoI,
	fromCoI, 
	mkClassPPredCoI, mkIParamPredCoI, mkEqPredCoI

       ) where 

#include "HsVersions.h"

import TypeRep
import Type
import TyCon
import Class
import Var
import VarEnv
import VarSet
import Name
import PrelNames
import Util
import BasicTypes
import Outputable
import FastString
\end{code}

%************************************************************************
%*									*
	Functions over Kinds		
%*									*
%************************************************************************

\begin{code}
-- | Essentially 'funResultTy' on kinds
kindFunResult :: Kind -> Kind
kindFunResult k = funResultTy k

kindAppResult :: Kind -> [arg] -> Kind
kindAppResult k []     = k
kindAppResult k (_:as) = kindAppResult (kindFunResult k) as

-- | Essentially 'splitFunTys' on kinds
splitKindFunTys :: Kind -> ([Kind],Kind)
splitKindFunTys k = splitFunTys k

splitKindFunTy_maybe :: Kind -> Maybe (Kind,Kind)
splitKindFunTy_maybe = splitFunTy_maybe

-- | Essentially 'splitFunTysN' on kinds
splitKindFunTysN :: Int -> Kind -> ([Kind],Kind)
splitKindFunTysN k = splitFunTysN k

-- | Find the result 'Kind' of a type synonym, 
-- after applying it to its 'arity' number of type variables
-- Actually this function works fine on data types too, 
-- but they'd always return '*', so we never need to ask
synTyConResKind :: TyCon -> Kind
synTyConResKind tycon = kindAppResult (tyConKind tycon) (tyConTyVars tycon)

-- | See "Type#kind_subtyping" for details of the distinction between these 'Kind's
isUbxTupleKind, isOpenTypeKind, isArgTypeKind, isUnliftedTypeKind :: Kind -> Bool
isOpenTypeKindCon, isUbxTupleKindCon, isArgTypeKindCon,
        isUnliftedTypeKindCon, isSubArgTypeKindCon      :: TyCon -> Bool

isOpenTypeKindCon tc    = tyConUnique tc == openTypeKindTyConKey

isOpenTypeKind (TyConApp tc _) = isOpenTypeKindCon tc
isOpenTypeKind _               = False

isUbxTupleKindCon tc = tyConUnique tc == ubxTupleKindTyConKey

isUbxTupleKind (TyConApp tc _) = isUbxTupleKindCon tc
isUbxTupleKind _               = False

isArgTypeKindCon tc = tyConUnique tc == argTypeKindTyConKey

isArgTypeKind (TyConApp tc _) = isArgTypeKindCon tc
isArgTypeKind _               = False

isUnliftedTypeKindCon tc = tyConUnique tc == unliftedTypeKindTyConKey

isUnliftedTypeKind (TyConApp tc _) = isUnliftedTypeKindCon tc
isUnliftedTypeKind _               = False

isSubOpenTypeKind :: Kind -> Bool
-- ^ True of any sub-kind of OpenTypeKind (i.e. anything except arrow)
isSubOpenTypeKind (FunTy k1 k2)    = ASSERT2 ( isKind k1, text "isSubOpenTypeKind" <+> ppr k1 <+> text "::" <+> ppr (typeKind k1) ) 
                                     ASSERT2 ( isKind k2, text "isSubOpenTypeKind" <+> ppr k2 <+> text "::" <+> ppr (typeKind k2) ) 
                                     False
isSubOpenTypeKind (TyConApp kc []) = ASSERT( isKind (TyConApp kc []) ) True
isSubOpenTypeKind other            = ASSERT( isKind other ) False
         -- This is a conservative answer
         -- It matters in the call to isSubKind in
	 -- checkExpectedKind.

isSubArgTypeKindCon kc
  | isUnliftedTypeKindCon kc = True
  | isLiftedTypeKindCon kc   = True
  | isArgTypeKindCon kc      = True
  | otherwise                = False

isSubArgTypeKind :: Kind -> Bool
-- ^ True of any sub-kind of ArgTypeKind 
isSubArgTypeKind (TyConApp kc []) = isSubArgTypeKindCon kc
isSubArgTypeKind _                = False

-- | Is this a super-kind (i.e. a type-of-kinds)?
isSuperKind :: Type -> Bool
isSuperKind (TyConApp (skc) []) = isSuperKindTyCon skc
isSuperKind _                   = False

-- | Is this a kind (i.e. a type-of-types)?
isKind :: Kind -> Bool
isKind k = isSuperKind (typeKind k)

isSubKind :: Kind -> Kind -> Bool
-- ^ @k1 \`isSubKind\` k2@ checks that @k1@ <: @k2@
isSubKind (TyConApp kc1 []) (TyConApp kc2 []) = kc1 `isSubKindCon` kc2
isSubKind (FunTy a1 r1) (FunTy a2 r2)	      = (a2 `isSubKind` a1) && (r1 `isSubKind` r2)
isSubKind (PredTy (EqPred ty1 ty2)) (PredTy (EqPred ty1' ty2')) 
  = ty1 `tcEqType` ty1' && ty2 `tcEqType` ty2'
isSubKind _             _                     = False

eqKind :: Kind -> Kind -> Bool
eqKind = tcEqType

isSubKindCon :: TyCon -> TyCon -> Bool
-- ^ @kc1 \`isSubKindCon\` kc2@ checks that @kc1@ <: @kc2@
isSubKindCon kc1 kc2
  | isLiftedTypeKindCon kc1   && isLiftedTypeKindCon kc2   = True
  | isUnliftedTypeKindCon kc1 && isUnliftedTypeKindCon kc2 = True
  | isUbxTupleKindCon kc1     && isUbxTupleKindCon kc2     = True
  | isOpenTypeKindCon kc2                                  = True 
                           -- we already know kc1 is not a fun, its a TyCon
  | isArgTypeKindCon kc2      && isSubArgTypeKindCon kc1   = True
  | otherwise                                              = False

defaultKind :: Kind -> Kind
-- ^ Used when generalising: default kind ? and ?? to *. See "Type#kind_subtyping" for more
-- information on what that means

-- When we generalise, we make generic type variables whose kind is
-- simple (* or *->* etc).  So generic type variables (other than
-- built-in constants like 'error') always have simple kinds.  This is important;
-- consider
--	f x = True
-- We want f to get type
--	f :: forall (a::*). a -> Bool
-- Not 
--	f :: forall (a::??). a -> Bool
-- because that would allow a call like (f 3#) as well as (f True),
--and the calling conventions differ.  This defaulting is done in TcMType.zonkTcTyVarBndr.
defaultKind k 
  | isSubOpenTypeKind k = liftedTypeKind
  | isSubArgTypeKind k  = liftedTypeKind
  | otherwise        = k
\end{code}

%************************************************************************
%*									*
            Coercions
%*									*
%************************************************************************


\begin{code}
-- | A 'Coercion' represents a 'Type' something should be coerced to.
type Coercion     = Type

-- | A 'CoercionKind' is always of form @ty1 ~ ty2@ and indicates the
-- types that a 'Coercion' will work on.
type CoercionKind = Kind

------------------------------

-- | This breaks a 'Coercion' with 'CoercionKind' @T A B C ~ T D E F@ into
-- a list of 'Coercion's of kinds @A ~ D@, @B ~ E@ and @E ~ F@. Hence:
--
-- > decomposeCo 3 c = [right (left (left c)), right (left c), right c]
decomposeCo :: Arity -> Coercion -> [Coercion]
decomposeCo n co
  = go n co []
  where
    go 0 _  cos = cos
    go n co cos = go (n-1) (mkLeftCoercion co)
			   (mkRightCoercion co : cos)


-------------------------------------------------------
-- and some coercion kind stuff

coVarKind :: CoVar -> (Type,Type) 
-- c :: t1 ~ t2
coVarKind cv = case coVarKind_maybe cv of
                 Just ts -> ts
                 Nothing -> pprPanic "coVarKind" (ppr cv $$ ppr (tyVarKind cv))

coVarKind_maybe :: CoVar -> Maybe (Type,Type) 
coVarKind_maybe cv = splitCoKind_maybe (tyVarKind cv)

-- | Take a 'CoercionKind' apart into the two types it relates: see also 'mkCoKind'.
-- Panics if the argument is not a valid 'CoercionKind'
splitCoKind_maybe :: Kind -> Maybe (Type, Type)
splitCoKind_maybe co | Just co' <- kindView co = splitCoKind_maybe co'
splitCoKind_maybe (PredTy (EqPred ty1 ty2))    = Just (ty1, ty2)
splitCoKind_maybe _                            = Nothing

-- | Makes a 'CoercionKind' from two types: the types whose equality 
-- is proven by the relevant 'Coercion'
mkCoKind :: Type -> Type -> CoercionKind
mkCoKind ty1 ty2 = PredTy (EqPred ty1 ty2)

-- | (mkCoPredTy s t r) produces the type:   (s~t) => r
mkCoPredTy :: Type -> Type -> Type -> Type
mkCoPredTy s t r = ASSERT( not (co_var `elemVarSet` tyVarsOfType r) )
                   ForAllTy co_var r
  where
    co_var = mkWildCoVar (mkCoKind s t)

splitCoPredTy_maybe :: Type -> Maybe (Type, Type, Type)
splitCoPredTy_maybe ty
  | Just (cv,r) <- splitForAllTy_maybe ty
  , isCoVar cv
  , Just (s,t) <- coVarKind_maybe cv
  = Just (s,t,r)
  | otherwise
  = Nothing

-- | Tests whether a type is just a type equality predicate
isEqPredTy :: Type -> Bool
isEqPredTy (PredTy pred) = isEqPred pred
isEqPredTy _             = False

-- | Creates a type equality predicate
mkEqPred :: (Type, Type) -> PredType
mkEqPred (ty1, ty2) = EqPred ty1 ty2

-- | Splits apart a type equality predicate, if the supplied 'PredType' is one.
-- Panics otherwise
getEqPredTys :: PredType -> (Type,Type)
getEqPredTys (EqPred ty1 ty2) = (ty1, ty2)
getEqPredTys other	      = pprPanic "getEqPredTys" (ppr other)

isIdentityCoercion :: Coercion -> Bool
isIdentityCoercion co  
  = case coercionKind co of
       (t1,t2) -> t1 `coreEqType` t2
\end{code}

%************************************************************************
%*									*
            Building coercions
%*									*
%************************************************************************

Coercion kind and type mk's (make saturated TyConApp CoercionTyCon{...} args)

\begin{code}
-- | Make a coercion from the specified coercion 'TyCon' and the 'Type' arguments to
-- that coercion. Try to use the @mk*Coercion@ family of functions instead of using this function
-- if possible
mkCoercion :: TyCon -> [Type] -> Coercion
mkCoercion coCon args = ASSERT( tyConArity coCon == length args ) 
                        TyConApp coCon args

mkCoVarCoercion :: CoVar -> Coercion 
mkCoVarCoercion cv = mkTyVarTy cv 

-- | Apply a 'Coercion' to another 'Coercion', which is presumably a
-- 'Coercion' constructor of some kind
mkAppCoercion :: Coercion -> Coercion -> Coercion
mkAppCoercion co1 co2 = mkAppTy co1 co2

-- | Applies multiple 'Coercion's to another 'Coercion', from left to right.
-- See also 'mkAppCoercion'
mkAppsCoercion :: Coercion -> [Coercion] -> Coercion
mkAppsCoercion co1 tys = foldl mkAppTy co1 tys

-- | Apply a type constructor to a list of coercions.
mkTyConCoercion :: TyCon -> [Coercion] -> Coercion
mkTyConCoercion con cos = mkTyConApp con cos

-- | Make a function 'Coercion' between two other 'Coercion's
mkFunCoercion :: Coercion -> Coercion -> Coercion
mkFunCoercion co1 co2 = mkFunTy co1 co2

-- | Make a 'Coercion' which binds a variable within an inner 'Coercion'
mkForAllCoercion :: Var -> Coercion -> Coercion
-- note that a TyVar should be used here, not a CoVar (nor a TcTyVar)
mkForAllCoercion tv  co  = ASSERT ( isTyCoVar tv ) mkForAllTy tv co


-------------------------------

mkSymCoercion :: Coercion -> Coercion
-- ^ Create a symmetric version of the given 'Coercion' that asserts equality
-- between the same types but in the other "direction", so a kind of @t1 ~ t2@ 
-- becomes the kind @t2 ~ t1@.
mkSymCoercion g = mkCoercion symCoercionTyCon [g]

mkTransCoercion :: Coercion -> Coercion -> Coercion
-- ^ Create a new 'Coercion' by exploiting transitivity on the two given 'Coercion's.
mkTransCoercion g1 g2 = mkCoercion transCoercionTyCon [g1, g2]

mkLeftCoercion :: Coercion -> Coercion
-- ^ From an application 'Coercion' build a 'Coercion' that asserts the equality of 
-- the "functions" on either side of the type equality. So if @c@ has kind @f x ~ g y@ then:
--
-- > mkLeftCoercion c :: f ~ g
mkLeftCoercion co = mkCoercion leftCoercionTyCon [co]

mkRightCoercion :: Coercion -> Coercion
-- ^ From an application 'Coercion' build a 'Coercion' that asserts the equality of 
-- the "arguments" on either side of the type equality. So if @c@ has kind @f x ~ g y@ then:
--
-- > mkLeftCoercion c :: x ~ y
mkRightCoercion co = mkCoercion rightCoercionTyCon [co]

mkCsel1Coercion, mkCsel2Coercion, mkCselRCoercion :: Coercion -> Coercion
mkCsel1Coercion co = mkCoercion csel1CoercionTyCon [co]
mkCsel2Coercion co = mkCoercion csel2CoercionTyCon [co]
mkCselRCoercion co = mkCoercion cselRCoercionTyCon [co]

-------------------------------
mkInstCoercion :: Coercion -> Type -> Coercion
-- ^ Instantiates a 'Coercion' with a 'Type' argument. If possible, it immediately performs
-- the resulting beta-reduction, otherwise it creates a suspended instantiation.
mkInstCoercion co ty = mkCoercion instCoercionTyCon  [co, ty]

mkInstsCoercion :: Coercion -> [Type] -> Coercion
-- ^ As 'mkInstCoercion', but instantiates the coercion with a number of type arguments, left-to-right
mkInstsCoercion co tys = foldl mkInstCoercion co tys

-- | Manufacture a coercion from this air. Needless to say, this is not usually safe,
-- but it is used when we know we are dealing with bottom, which is one case in which 
-- it is safe.  This is also used implement the @unsafeCoerce#@ primitive.
-- Optimise by pushing down through type constructors
mkUnsafeCoercion :: Type -> Type -> Coercion
mkUnsafeCoercion (TyConApp tc1 tys1) (TyConApp tc2 tys2)
  | tc1 == tc2
  = TyConApp tc1 (zipWith mkUnsafeCoercion tys1 tys2)

mkUnsafeCoercion (FunTy a1 r1) (FunTy a2 r2)
  = FunTy (mkUnsafeCoercion a1 a2) (mkUnsafeCoercion r1 r2)

mkUnsafeCoercion ty1 ty2 
  | ty1 `coreEqType` ty2 = ty1
  | otherwise            = mkCoercion unsafeCoercionTyCon [ty1, ty2]

-- See note [Newtype coercions] in TyCon

-- | Create a coercion suitable for the given 'TyCon'. The 'Name' should be that of a
-- new coercion 'TyCon', the 'TyVar's the arguments expected by the @newtype@ and the
-- type the appropriate right hand side of the @newtype@, with the free variables
-- a subset of those 'TyVar's.
mkNewTypeCoercion :: Name -> TyCon -> [TyVar] -> Type -> TyCon
mkNewTypeCoercion name tycon tvs rhs_ty
  = mkCoercionTyCon name arity desc
  where
    arity = length tvs
    desc = CoAxiom { co_ax_tvs = tvs 
                   , co_ax_lhs = mkTyConApp tycon (mkTyVarTys tvs)
                   , co_ax_rhs = rhs_ty }

-- | Create a coercion identifying a @data@, @newtype@ or @type@ representation type
-- and its family instance.  It has the form @Co tvs :: F ts ~ R tvs@, where @Co@ is 
-- the coercion tycon built here, @F@ the family tycon and @R@ the (derived)
-- representation tycon.
mkFamInstCoercion :: Name	-- ^ Unique name for the coercion tycon
		  -> [TyVar]	-- ^ Type parameters of the coercion (@tvs@)
		  -> TyCon	-- ^ Family tycon (@F@)
		  -> [Type]	-- ^ Type instance (@ts@)
		  -> TyCon	-- ^ Representation tycon (@R@)
		  -> TyCon	-- ^ Coercion tycon (@Co@)
mkFamInstCoercion name tvs family inst_tys rep_tycon
  = mkCoercionTyCon name arity desc
  where
    arity = length tvs
    desc = CoAxiom { co_ax_tvs = tvs
                   , co_ax_lhs = mkTyConApp family inst_tys 
                   , co_ax_rhs = mkTyConApp rep_tycon (mkTyVarTys tvs) }
\end{code}


%************************************************************************
%*									*
            Coercion Type Constructors
%*									*
%************************************************************************

Example.  The coercion ((sym c) (sym d) (sym e))
will be represented by (TyConApp sym [c, sym d, sym e])
If sym c :: p1=q1
   sym d :: p2=q2
   sym e :: p3=q3
then ((sym c) (sym d) (sym e)) :: (p1 p2 p3)=(q1 q2 q3)

\begin{code}
-- | Coercion type constructors: avoid using these directly and instead use 
-- the @mk*Coercion@ and @split*Coercion@ family of functions if possible.
--
-- Each coercion TyCon is built with the special CoercionTyCon record and
-- carries its own kinding rule.  Such CoercionTyCons must be fully applied
-- by any TyConApp in which they are applied, however they may also be over
-- applied (see example above) and the kinding function must deal with this.
symCoercionTyCon, transCoercionTyCon, leftCoercionTyCon, 
  rightCoercionTyCon, instCoercionTyCon, unsafeCoercionTyCon,
  csel1CoercionTyCon, csel2CoercionTyCon, cselRCoercionTyCon :: TyCon

symCoercionTyCon    = mkCoercionTyCon symCoercionTyConName   1 CoSym
transCoercionTyCon  = mkCoercionTyCon transCoercionTyConName 2 CoTrans
leftCoercionTyCon   = mkCoercionTyCon leftCoercionTyConName  1 CoLeft
rightCoercionTyCon  = mkCoercionTyCon rightCoercionTyConName 1 CoRight
instCoercionTyCon   =  mkCoercionTyCon instCoercionTyConName 2 CoInst
csel1CoercionTyCon  = mkCoercionTyCon csel1CoercionTyConName 1 CoCsel1
csel2CoercionTyCon  = mkCoercionTyCon csel2CoercionTyConName 1 CoCsel2
cselRCoercionTyCon  = mkCoercionTyCon cselRCoercionTyConName 1 CoCselR
unsafeCoercionTyCon = mkCoercionTyCon unsafeCoercionTyConName 2 CoUnsafe

transCoercionTyConName, symCoercionTyConName, leftCoercionTyConName, 
   rightCoercionTyConName, instCoercionTyConName, unsafeCoercionTyConName,
   csel1CoercionTyConName, csel2CoercionTyConName, cselRCoercionTyConName :: Name

transCoercionTyConName 	= mkCoConName (fsLit "trans") transCoercionTyConKey transCoercionTyCon
symCoercionTyConName   	= mkCoConName (fsLit "sym") symCoercionTyConKey symCoercionTyCon
leftCoercionTyConName  	= mkCoConName (fsLit "left") leftCoercionTyConKey leftCoercionTyCon
rightCoercionTyConName 	= mkCoConName (fsLit "right") rightCoercionTyConKey rightCoercionTyCon
instCoercionTyConName  	= mkCoConName (fsLit "inst") instCoercionTyConKey instCoercionTyCon
csel1CoercionTyConName  = mkCoConName (fsLit "csel1") csel1CoercionTyConKey csel1CoercionTyCon
csel2CoercionTyConName  = mkCoConName (fsLit "csel2") csel2CoercionTyConKey csel2CoercionTyCon
cselRCoercionTyConName  = mkCoConName (fsLit "cselR") cselRCoercionTyConKey cselRCoercionTyCon
unsafeCoercionTyConName = mkCoConName (fsLit "CoUnsafe") unsafeCoercionTyConKey unsafeCoercionTyCon

mkCoConName :: FastString -> Unique -> TyCon -> Name
mkCoConName occ key coCon = mkWiredInName gHC_PRIM (mkTcOccFS occ)
                            key (ATyCon coCon) BuiltInSyntax
\end{code}

\begin{code}
------------
decompLR_maybe :: (Type,Type) -> Maybe ((Type,Type), (Type,Type))
-- Helper for left and right.  Finds coercion kind of its input and
-- returns the left and right projections of the coercion...
--
-- if c :: t1 s1 ~ t2 s2 then splitCoercionKindOf c = ((t1, t2), (s1, s2))
decompLR_maybe (ty1,ty2)
  | Just (ty_fun1, ty_arg1) <- splitAppTy_maybe ty1
  , Just (ty_fun2, ty_arg2) <- splitAppTy_maybe ty2
  = Just ((ty_fun1, ty_fun2),(ty_arg1, ty_arg2))
decompLR_maybe _ = Nothing

------------
decompInst_maybe :: (Type, Type) -> Maybe ((TyVar,TyVar), (Type,Type))
decompInst_maybe (ty1, ty2)
  | Just (tv1,r1) <- splitForAllTy_maybe ty1
  , Just (tv2,r2) <- splitForAllTy_maybe ty2
  = Just ((tv1,tv2), (r1,r2))
decompInst_maybe _ = Nothing

------------
decompCsel_maybe :: (Type, Type) -> Maybe ((Type,Type), (Type,Type), (Type,Type))
--   If         co :: (s1~t1 => r1) ~ (s2~t2 => r2)
-- Then   csel1 co ::            s1 ~ s2
--        csel2 co :: 		 t1 ~ t2
--        cselR co :: 		 r1 ~ r2
decompCsel_maybe (ty1, ty2)
  | Just (s1, t1, r1) <- splitCoPredTy_maybe ty1
  , Just (s2, t2, r2) <- splitCoPredTy_maybe ty2
  = Just ((s1,s2), (t1,t2), (r1,r2))
decompCsel_maybe _ = Nothing
\end{code}


%************************************************************************
%*									*
            Newtypes
%*									*
%************************************************************************

\begin{code}
instNewTyCon_maybe :: TyCon -> [Type] -> Maybe (Type, CoercionI)
-- ^ If @co :: T ts ~ rep_ty@ then:
--
-- > instNewTyCon_maybe T ts = Just (rep_ty, co)
instNewTyCon_maybe tc tys
  | Just (tvs, ty, mb_co_tc) <- unwrapNewTyCon_maybe tc
  = ASSERT( tys `lengthIs` tyConArity tc )
    Just (substTyWith tvs tys ty, 
	  case mb_co_tc of
	     Nothing    -> IdCo (mkTyConApp tc    tys)
	     Just co_tc -> ACo  (mkTyConApp co_tc tys))
  | otherwise
  = Nothing

-- this is here to avoid module loops
splitNewTypeRepCo_maybe :: Type -> Maybe (Type, Coercion)  
-- ^ Sometimes we want to look through a @newtype@ and get its associated coercion.
-- This function only strips *one layer* of @newtype@ off, so the caller will usually call
-- itself recursively. Furthermore, this function should only be applied to types of kind @*@,
-- hence the newtype is always saturated. If @co : ty ~ ty'@ then:
--
-- > splitNewTypeRepCo_maybe ty = Just (ty', co)
--
-- The function returns @Nothing@ for non-@newtypes@ or fully-transparent @newtype@s.
splitNewTypeRepCo_maybe ty 
  | Just ty' <- coreView ty = splitNewTypeRepCo_maybe ty'
splitNewTypeRepCo_maybe (TyConApp tc tys)
  | Just (ty', coi) <- instNewTyCon_maybe tc tys
  = case coi of
	ACo co -> Just (ty', co)
	IdCo _ -> panic "splitNewTypeRepCo_maybe"
			-- This case handled by coreView
splitNewTypeRepCo_maybe _
  = Nothing

-- | Determines syntactic equality of coercions
coreEqCoercion :: Coercion -> Coercion -> Bool
coreEqCoercion = coreEqType

coreEqCoercion2 :: RnEnv2 -> Coercion -> Coercion -> Bool
coreEqCoercion2 = coreEqType2
\end{code}


%************************************************************************
%*									*
            CoercionI and its constructors
%*									*
%************************************************************************

--------------------------------------
-- CoercionI smart constructors
--	lifted smart constructors of ordinary coercions

\begin{code}
-- | 'CoercionI' represents a /lifted/ ordinary 'Coercion', in that it
-- can represent either one of:
--
-- 1. A proper 'Coercion'
--
-- 2. The identity coercion
data CoercionI = IdCo Type | ACo Coercion

liftCoI :: (Type -> Type) -> CoercionI -> CoercionI
liftCoI f (IdCo ty) = IdCo (f ty)
liftCoI f (ACo ty)  = ACo (f ty)

liftCoI2 :: (Type -> Type -> Type) -> CoercionI -> CoercionI -> CoercionI
liftCoI2 f (IdCo ty1) (IdCo ty2) = IdCo (f ty1 ty2)
liftCoI2 f coi1       coi2       = ACo (f (fromCoI coi1) (fromCoI coi2))

liftCoIs :: ([Type] -> Type) -> [CoercionI] -> CoercionI
liftCoIs f cois = go_id [] cois
  where
    go_id rev_tys []               = IdCo (f (reverse rev_tys))
    go_id rev_tys (IdCo ty : cois) = go_id  (ty:rev_tys) cois
    go_id rev_tys (ACo  co : cois) = go_aco (co:rev_tys) cois

    go_aco rev_tys []               = ACo (f (reverse rev_tys))
    go_aco rev_tys (IdCo ty : cois) = go_aco (ty:rev_tys) cois
    go_aco rev_tys (ACo  co : cois) = go_aco (co:rev_tys) cois

instance Outputable CoercionI where
  ppr (IdCo _) = ptext (sLit "IdCo")
  ppr (ACo co) = ppr co

isIdentityCoI :: CoercionI -> Bool
isIdentityCoI (IdCo _) = True
isIdentityCoI (ACo _)  = False

-- | Return either the 'Coercion' contained within the 'CoercionI' or the given
-- 'Type' if the 'CoercionI' is the identity 'Coercion'
fromCoI :: CoercionI -> Type
fromCoI (IdCo ty) = ty	-- Identity coercion represented 
fromCoI (ACo co)  = co	-- 	by the type itself

-- | Smart constructor for @sym@ on 'CoercionI', see also 'mkSymCoercion'
mkSymCoI :: CoercionI -> CoercionI
mkSymCoI (IdCo ty) = IdCo ty
mkSymCoI (ACo co)  = ACo $ mkCoercion symCoercionTyCon [co] 
				-- the smart constructor
				-- is too smart with tyvars

-- | Smart constructor for @trans@ on 'CoercionI', see also 'mkTransCoercion'
mkTransCoI :: CoercionI -> CoercionI -> CoercionI
mkTransCoI (IdCo _) aco = aco
mkTransCoI aco (IdCo _) = aco
mkTransCoI (ACo co1) (ACo co2) = ACo $ mkTransCoercion co1 co2

-- | Smart constructor for type constructor application on 'CoercionI', see also 'mkAppCoercion'
mkTyConAppCoI :: TyCon -> [CoercionI] -> CoercionI
mkTyConAppCoI tyCon cois = liftCoIs (mkTyConApp tyCon) cois

-- | Smart constructor for honest-to-god 'Coercion' application on 'CoercionI', see also 'mkAppCoercion'
mkAppTyCoI :: CoercionI -> CoercionI -> CoercionI
mkAppTyCoI = liftCoI2 mkAppTy

mkFunTyCoI :: CoercionI -> CoercionI -> CoercionI
mkFunTyCoI = liftCoI2 mkFunTy

-- | Smart constructor for quantified 'Coercion's on 'CoercionI', see also 'mkForAllCoercion'
mkForAllTyCoI :: TyVar -> CoercionI -> CoercionI
mkForAllTyCoI tv = liftCoI (ForAllTy tv)

-- | Smart constructor for class 'Coercion's on 'CoercionI'. Satisfies:
--
-- > mkClassPPredCoI cls tys cois :: PredTy (cls tys) ~ PredTy (cls (tys `cast` cois))
mkClassPPredCoI :: Class -> [CoercionI] -> CoercionI
mkClassPPredCoI cls = liftCoIs (PredTy . ClassP cls)

-- | Smart constructor for implicit parameter 'Coercion's on 'CoercionI'. Similar to 'mkClassPPredCoI'
mkIParamPredCoI :: (IPName Name) -> CoercionI -> CoercionI 
mkIParamPredCoI ipn = liftCoI (PredTy . IParam ipn)

-- | Smart constructor for type equality 'Coercion's on 'CoercionI'. Similar to 'mkClassPPredCoI'
mkEqPredCoI :: CoercionI -> CoercionI -> CoercionI
mkEqPredCoI = liftCoI2 (\t1 t2 -> PredTy (EqPred t1 t2))
\end{code}

%************************************************************************
%*									*
	     The kind of a type, and of a coercion
%*									*
%************************************************************************

\begin{code}
typeKind :: Type -> Kind
typeKind ty@(TyConApp tc tys) 
  | isCoercionTyCon tc = typeKind (fst (coercionKind ty))
  | otherwise          = kindAppResult (tyConKind tc) tys
	-- During coercion optimisation we *do* match a type
	-- against a coercion (see OptCoercion.matchesAxiomLhs)
	-- So the use of typeKind in Unify.match_kind must work on coercions too
	-- Hence the isCoercionTyCon case above

typeKind (PredTy pred)	      = predKind pred
typeKind (AppTy fun _)        = kindFunResult (typeKind fun)
typeKind (ForAllTy _ ty)      = typeKind ty
typeKind (TyVarTy tyvar)      = tyVarKind tyvar
typeKind (FunTy _arg res)
    -- Hack alert.  The kind of (Int -> Int#) is liftedTypeKind (*), 
    --              not unliftedTypKind (#)
    -- The only things that can be after a function arrow are
    --   (a) types (of kind openTypeKind or its sub-kinds)
    --   (b) kinds (of super-kind TY) (e.g. * -> (* -> *))
    | isTySuperKind k         = k
    | otherwise               = ASSERT( isSubOpenTypeKind k) liftedTypeKind 
    where
      k = typeKind res

------------------
predKind :: PredType -> Kind
predKind (EqPred {}) = coSuperKind	-- A coercion kind!
predKind (ClassP {}) = liftedTypeKind	-- Class and implicitPredicates are
predKind (IParam {}) = liftedTypeKind 	-- always represented by lifted types

------------------
-- | If it is the case that
--
-- > c :: (t1 ~ t2)
--
-- i.e. the kind of @c@ is a 'CoercionKind' relating @t1@ and @t2@, 
-- then @coercionKind c = (t1, t2)@.
coercionKind :: Coercion -> (Type, Type)
coercionKind ty@(TyVarTy a) | isCoVar a = coVarKind a
                            | otherwise = (ty, ty)
coercionKind (AppTy ty1 ty2) 
  = let (s1, t1) = coercionKind ty1
        (s2, t2) = coercionKind ty2 in
    (mkAppTy s1 s2, mkAppTy t1 t2)
coercionKind co@(TyConApp tc args)
  | Just (ar, desc) <- isCoercionTyCon_maybe tc 
    -- CoercionTyCons carry their kinding rule, so we use it here
  = WARN( not (length args >= ar), ppr co )	-- Always saturated
    (let (ty1,  ty2)  = coTyConAppKind desc (take ar args)
	 (tys1, tys2) = coercionKinds (drop ar args)
     in (mkAppTys ty1 tys1, mkAppTys ty2 tys2))

  | otherwise
  = let (lArgs, rArgs) = coercionKinds args in
    (TyConApp tc lArgs, TyConApp tc rArgs)

coercionKind (FunTy ty1 ty2) 
  = let (t1, t2) = coercionKind ty1
        (s1, s2) = coercionKind ty2 in
    (mkFunTy t1 s1, mkFunTy t2 s2)

coercionKind (ForAllTy tv ty)
  | isCoVar tv
--     c1 :: s1~s2  c2 :: t1~t2   c3 :: r1~r2
--    ----------------------------------------------
--    c1~c2 => c3  ::  (s1~t1) => r1 ~ (s2~t2) => r2
--      or
--    forall (_:c1~c2)
  = let (c1,c2) = coVarKind tv
    	(s1,s2) = coercionKind c1
    	(t1,t2) = coercionKind c2
    	(r1,r2) = coercionKind ty
    in
    (mkCoPredTy s1 t1 r1, mkCoPredTy s2 t2 r2)

  | otherwise
--     c1 :: s1~s2  c2 :: t1~t2   c3 :: r1~r2
--   ----------------------------------------------
--    forall a:k. c :: forall a:k. t1 ~ forall a:k. t2
  = let (ty1, ty2) = coercionKind ty in
    (ForAllTy tv ty1, ForAllTy tv ty2)

coercionKind (PredTy (ClassP cl args)) 
  = let (lArgs, rArgs) = coercionKinds args in
    (PredTy (ClassP cl lArgs), PredTy (ClassP cl rArgs))
coercionKind (PredTy (IParam name ty))
  = let (ty1, ty2) = coercionKind ty in
    (PredTy (IParam name ty1), PredTy (IParam name ty2))
coercionKind (PredTy (EqPred c1 c2)) 
  = pprTrace "coercionKind" (pprEqPred (c1,c2)) $
  -- These should not show up in coercions at all
  -- becuase they are in the form of for-alls
    let k1 = coercionKindPredTy c1
        k2 = coercionKindPredTy c2 in
    (k1,k2)
  where
    coercionKindPredTy c = let (t1, t2) = coercionKind c in mkCoKind t1 t2

------------------
-- | Apply 'coercionKind' to multiple 'Coercion's
coercionKinds :: [Coercion] -> ([Type], [Type])
coercionKinds tys = unzip $ map coercionKind tys

------------------
-- | 'coTyConAppKind' is given a list of the type arguments to the 'CoTyCon',
-- and constructs the types that the resulting coercion relates.
-- Fails (in the monad) if ill-kinded.
-- Typically the monad is 
--   either the Lint monad (with the consistency-check flag = True), 
--   or the ID monad with a panic on failure (and the consistency-check flag = False)
coTyConAppKind 
    :: CoTyConDesc
    -> [Type]	  		-- Exactly right number of args
    -> (Type, Type)		-- Kind of this application
coTyConAppKind CoUnsafe (ty1:ty2:_)
  = (ty1,ty2)
coTyConAppKind CoSym (co:_) 
  | (ty1,ty2) <- coercionKind co = (ty2,ty1)
coTyConAppKind CoTrans (co1:co2:_) 
  = (fst (coercionKind co1), snd (coercionKind co2))
coTyConAppKind CoLeft (co:_) 
  | Just (res,_) <- decompLR_maybe (coercionKind co) = res
coTyConAppKind CoRight (co:_) 
  | Just (_,res) <- decompLR_maybe (coercionKind co) = res
coTyConAppKind CoCsel1 (co:_) 
  | Just (res,_,_) <- decompCsel_maybe (coercionKind co) = res
coTyConAppKind CoCsel2 (co:_) 
  | Just (_,res,_) <- decompCsel_maybe (coercionKind co) = res
coTyConAppKind CoCselR (co:_) 
  | Just (_,_,res) <- decompCsel_maybe (coercionKind co) = res
coTyConAppKind CoInst (co:ty:_) 
  | Just ((tv1,tv2), (ty1,ty2)) <- decompInst_maybe (coercionKind co)
  = (substTyWith [tv1] [ty] ty1, substTyWith [tv2] [ty] ty2) 
coTyConAppKind (CoAxiom { co_ax_tvs = tvs 
                        , co_ax_lhs = lhs_ty, co_ax_rhs = rhs_ty }) cos
  = (substTyWith tvs tys1 lhs_ty, substTyWith tvs tys2 rhs_ty)
  where
    (tys1, tys2) = coercionKinds cos
coTyConAppKind desc cos = pprTrace "coTyConAppKind" (ppr desc $$ braces (vcat 
                             [ ppr co <+> dcolon <+> pprEqPred (coercionKind co)
                             | co <- cos ])) $
                          coercionKind (head cos)
\end{code}
