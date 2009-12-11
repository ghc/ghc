%
% (c) The University of Glasgow 2006
%

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-- | Module for type coercions, as used in System FC. See 'CoreSyn.Expr' for
-- more on System FC and how coercions fit into it.
--
-- Coercions are represented as types, and their kinds tell what types the 
-- coercion works on. The coercion kind constructor is a special TyCon that must always be saturated, like so:
--
-- > typeKind (symCoercion type) :: TyConApp CoercionTyCon{...} [type, type]
module Coercion (
        -- * Main data type
        Coercion,
 
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

        splitNewTypeRepCo_maybe, instNewTyCon_maybe, decomposeCo,

        unsafeCoercionTyCon, symCoercionTyCon,
        transCoercionTyCon, leftCoercionTyCon, 
        rightCoercionTyCon, instCoercionTyCon, -- needed by TysWiredIn
        csel1CoercionTyCon, csel2CoercionTyCon, cselRCoercionTyCon, 

        -- ** Decomposition
        decompLR_maybe, decompCsel_maybe, decompInst_maybe,

        -- ** Optimisation
	optCoercion,

        -- ** Comparison
        coreEqCoercion, coreEqCoercion2,

	-- * CoercionI
	CoercionI(..),
	isIdentityCoI,
	mkSymCoI, mkTransCoI, 
	mkTyConAppCoI, mkAppTyCoI, mkFunTyCoI,
	mkForAllTyCoI,
	fromCoI, fromACo,
	mkClassPPredCoI, mkIParamPredCoI, mkEqPredCoI

       ) where 

#include "HsVersions.h"

import TypeRep
import Type
import TyCon
import Class
import Var
import VarEnv
import Name
import PrelNames
import Util
import Control.Monad
import BasicTypes
import MonadUtils
import Outputable
import FastString

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

------------------------------

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
mkCoPredTy s t r = ForAllTy (mkWildCoVar (mkCoKind s t)) r

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

-- | If it is the case that
--
-- > c :: (t1 ~ t2)
--
-- i.e. the kind of @c@ is a 'CoercionKind' relating @t1@ and @t2@, then @coercionKind c = (t1, t2)@.
coercionKind :: Coercion -> (Type, Type)
coercionKind ty@(TyVarTy a) | isCoVar a = coVarKind a
                            | otherwise = (ty, ty)
coercionKind (AppTy ty1 ty2) 
  = let (s1, t1) = coercionKind ty1
        (s2, t2) = coercionKind ty2 in
    (mkAppTy s1 s2, mkAppTy t1 t2)
coercionKind co@(TyConApp tc args)
  | Just (ar, rule) <- isCoercionTyCon_maybe tc 
    -- CoercionTyCons carry their kinding rule, so we use it here
  = WARN( not (length args >= ar), ppr co )	-- Always saturated
    (let (ty1,ty2) = runID (rule (return . typeKind)
                                (return . coercionKind)
				False (take ar args))
	     	       	      -- Apply the rule to the right number of args
    	     	       	      -- Always succeeds (if term is well-kinded!)
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

coercionKind (PredTy (EqPred c1 c2)) 
  = pprTrace "coercionKind" (pprEqPred (c1,c2)) $
    let k1 = coercionKindPredTy c1
        k2 = coercionKindPredTy c2 in
    (k1,k2)
  -- These should not show up in coercions at all
  -- becuase they are in the form of for-alls
  where
    coercionKindPredTy c = let (t1, t2) = coercionKind c in mkCoKind t1 t2



coercionKind (PredTy (ClassP cl args)) 
  = let (lArgs, rArgs) = coercionKinds args in
    (PredTy (ClassP cl lArgs), PredTy (ClassP cl rArgs))
coercionKind (PredTy (IParam name ty))
  = let (ty1, ty2) = coercionKind ty in
    (PredTy (IParam name ty1), PredTy (IParam name ty2))

-- | Apply 'coercionKind' to multiple 'Coercion's
coercionKinds :: [Coercion] -> ([Type], [Type])
coercionKinds tys = unzip $ map coercionKind tys

-------------------------------------
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
mkForAllCoercion tv  co  = ASSERT ( isTyVar tv ) mkForAllTy tv co


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
mkUnsafeCoercion :: Type -> Type -> Coercion
mkUnsafeCoercion ty1 ty2 
  = mkCoercion unsafeCoercionTyCon [ty1, ty2]


-- See note [Newtype coercions] in TyCon

-- | Create a coercion suitable for the given 'TyCon'. The 'Name' should be that of a
-- new coercion 'TyCon', the 'TyVar's the arguments expected by the @newtype@ and the
-- type the appropriate right hand side of the @newtype@, with the free variables
-- a subset of those 'TyVar's.
mkNewTypeCoercion :: Name -> TyCon -> [TyVar] -> Type -> TyCon
mkNewTypeCoercion name tycon tvs rhs_ty
  = mkCoercionTyCon name co_con_arity rule
  where
    co_con_arity = length tvs

    rule :: CoTyConKindChecker
    rule kc_ty kc_co checking args 
      = do { ks <- mapM kc_ty args
           ; unless (not checking || kindAppOk (tyConKind tycon) ks) 
                    (fail "Argument kind mis-match")
           ; return (TyConApp tycon args, substTyWith tvs args rhs_ty) }

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
mkFamInstCoercion name tvs family instTys rep_tycon
  = mkCoercionTyCon name coArity rule
  where
    coArity = length tvs

    rule :: CoTyConKindChecker
    rule kc_ty kc_co checking args 
      = do { ks <- mapM kc_ty args
           ; unless (not checking  || kindAppOk (tyConKind rep_tycon) ks)
                    (fail "Argument kind mis-match")
           ; return (substTyWith tvs args $	     -- with sigma = [tys/tvs],
		     TyConApp family instTys	     --       sigma (F ts)
		    , TyConApp rep_tycon args) }     --   ~ R tys

kindAppOk :: Kind -> [Kind] -> Bool
kindAppOk kfn [] = True
kindAppOk kfn (k:ks) 
  = case splitKindFunTy_maybe kfn of
      Just (kfa, kfb) | k `isSubKind` kfa -> kindAppOk kfb ks
      _other                              -> False
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

symCoercionTyCon 
  = mkCoercionTyCon symCoercionTyConName 1 kc_sym
  where
    kc_sym :: CoTyConKindChecker
    kc_sym kc_ty kc_co _ (co:_) 
      = do { (ty1,ty2) <- kc_co co
           ; return (ty2,ty1) }

transCoercionTyCon 
  = mkCoercionTyCon transCoercionTyConName 2 kc_trans
  where
    kc_trans :: CoTyConKindChecker
    kc_trans kc_ty kc_co checking (co1:co2:_)
      = do { (a1, r1) <- kc_co co1
           ; (a2, r2) <- kc_co co2 
	   ; unless (not checking || (r1 `coreEqType` a2))
                    (fail "Trans coercion mis-match")
           ; return (a1, r2) }

---------------------------------------------------
leftCoercionTyCon  = mkCoercionTyCon leftCoercionTyConName  1 (kcLR_help fst)
rightCoercionTyCon = mkCoercionTyCon rightCoercionTyConName 1 (kcLR_help snd)

kcLR_help :: (forall a. (a,a)->a) -> CoTyConKindChecker
kcLR_help select kc_ty kc_co _checking (co : _)
  = do { (ty1, ty2)  <- kc_co co
       ; case decompLR_maybe ty1 ty2 of
           Nothing  -> fail "decompLR" 
           Just res -> return (select res) }

decompLR_maybe :: Type -> Type -> Maybe ((Type,Type), (Type,Type))
-- Helper for left and right.  Finds coercion kind of its input and
-- returns the left and right projections of the coercion...
--
-- if c :: t1 s1 ~ t2 s2 then splitCoercionKindOf c = ((t1, t2), (s1, s2))
decompLR_maybe ty1 ty2
  | Just (ty_fun1, ty_arg1) <- splitAppTy_maybe ty1
  , Just (ty_fun2, ty_arg2) <- splitAppTy_maybe ty2
  = Just ((ty_fun1, ty_fun2),(ty_arg1, ty_arg2))
decompLR_maybe _ _ = Nothing

---------------------------------------------------
instCoercionTyCon 
  =  mkCoercionTyCon instCoercionTyConName 2 kcInst_help
  where
    kcInst_help :: CoTyConKindChecker
    kcInst_help kc_ty kc_co checking (co : ty : _)
      = do { (t1,t2) <- kc_co co
           ; k <- kc_ty ty
           ; case decompInst_maybe t1 t2 of
               Nothing -> fail "decompInst"
               Just ((tv1,tv2), (ty1,ty2)) -> do
           { unless (not checking || (k `isSubKind` tyVarKind tv1))
                    (fail "Coercion instantation kind mis-match")
           ; return (substTyWith [tv1] [ty] ty1,
                     substTyWith [tv2] [ty] ty2) } }

decompInst_maybe :: Type -> Type -> Maybe ((TyVar,TyVar), (Type,Type))
decompInst_maybe ty1 ty2
  | Just (tv1,r1) <- splitForAllTy_maybe ty1
  , Just (tv2,r2) <- splitForAllTy_maybe ty2
  = Just ((tv1,tv2), (r1,r2))


---------------------------------------------------
unsafeCoercionTyCon 
  = mkCoercionTyCon unsafeCoercionTyConName 2 kc_unsafe
  where
   kc_unsafe kc_ty kc_co _checking (ty1:ty2:_) 
    = do { k1 <- kc_ty ty1
         ; k2 <- kc_ty ty2
         ; return (ty1,ty2) }
        
---------------------------------------------------
-- The csel* family

csel1CoercionTyCon = mkCoercionTyCon csel1CoercionTyConName 1 (kcCsel_help fstOf3)
csel2CoercionTyCon = mkCoercionTyCon csel2CoercionTyConName 1 (kcCsel_help sndOf3)
cselRCoercionTyCon = mkCoercionTyCon cselRCoercionTyConName 1 (kcCsel_help thirdOf3) 

kcCsel_help :: (forall a. (a,a,a) -> a) -> CoTyConKindChecker
kcCsel_help select kc_ty kc_co _checking (co : rest)
  = do { (ty1,ty2) <- kc_co co
       ; case decompCsel_maybe ty1 ty2 of
           Nothing  -> fail "decompCsel"
           Just res -> return (select res) }

decompCsel_maybe :: Type -> Type -> Maybe ((Type,Type), (Type,Type), (Type,Type))
--   If         co :: (s1~t1 => r1) ~ (s2~t2 => r2)
-- Then   csel1 co ::            s1 ~ s2
--        csel2 co :: 		 t1 ~ t2
--        cselR co :: 		 r1 ~ r2
decompCsel_maybe ty1 ty2
  | Just (s1, t1, r1) <- splitCoPredTy_maybe ty1
  , Just (s2, t2, r2) <- splitCoPredTy_maybe ty2
  = Just ((s1,s2), (t1,t2), (r1,r2))
decompCsel_maybe _ _ = Nothing

fstOf3   :: (a,b,c) -> a    
sndOf3   :: (a,b,c) -> b    
thirdOf3 :: (a,b,c) -> c    
fstOf3      (a,_,_) =  a
sndOf3      (_,b,_) =  b
thirdOf3    (_,_,c) =  c

--------------------------------------
-- Their Names

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
	   Nothing    -> IdCo
	   Just co_tc -> ACo (mkTyConApp co_tc tys))
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
	IdCo   -> panic "splitNewTypeRepCo_maybe"
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
data CoercionI = IdCo | ACo Coercion

instance Outputable CoercionI where
  ppr IdCo     = ptext (sLit "IdCo")
  ppr (ACo co) = ppr co

isIdentityCoI :: CoercionI -> Bool
isIdentityCoI IdCo = True
isIdentityCoI _    = False

-- | Tests whether all the given 'CoercionI's represent the identity coercion
allIdCoIs :: [CoercionI] -> Bool
allIdCoIs = all isIdentityCoI

-- | For each 'CoercionI' in the input list, return either the 'Coercion' it
-- contains or the corresponding 'Type' from the other list
zipCoArgs :: [CoercionI] -> [Type] -> [Coercion]
zipCoArgs cois tys = zipWith fromCoI cois tys

-- | Return either the 'Coercion' contained within the 'CoercionI' or the given
-- 'Type' if the 'CoercionI' is the identity 'Coercion'
fromCoI :: CoercionI -> Type -> Type
fromCoI IdCo ty     = ty	-- Identity coercion represented 
fromCoI (ACo co) _  = co	-- 	by the type itself

-- | Smart constructor for @sym@ on 'CoercionI', see also 'mkSymCoercion'
mkSymCoI :: CoercionI -> CoercionI
mkSymCoI IdCo = IdCo
mkSymCoI (ACo co) = ACo $ mkCoercion symCoercionTyCon [co] 
				-- the smart constructor
				-- is too smart with tyvars

-- | Smart constructor for @trans@ on 'CoercionI', see also 'mkTransCoercion'
mkTransCoI :: CoercionI -> CoercionI -> CoercionI
mkTransCoI IdCo aco = aco
mkTransCoI aco IdCo = aco
mkTransCoI (ACo co1) (ACo co2) = ACo $ mkTransCoercion co1 co2

-- | Smart constructor for type constructor application on 'CoercionI', see also 'mkAppCoercion'
mkTyConAppCoI :: TyCon -> [Type] -> [CoercionI] -> CoercionI
mkTyConAppCoI tyCon tys cois
  | allIdCoIs cois = IdCo
  | otherwise	   = ACo (TyConApp tyCon (zipCoArgs cois tys))

-- | Smart constructor for honest-to-god 'Coercion' application on 'CoercionI', see also 'mkAppCoercion'
mkAppTyCoI :: Type -> CoercionI -> Type -> CoercionI -> CoercionI
mkAppTyCoI _   IdCo _   IdCo = IdCo
mkAppTyCoI ty1 coi1 ty2 coi2 =
	ACo $ AppTy (fromCoI coi1 ty1) (fromCoI coi2 ty2)


mkFunTyCoI :: Type -> CoercionI -> Type -> CoercionI -> CoercionI
mkFunTyCoI _   IdCo _   IdCo = IdCo
mkFunTyCoI ty1 coi1 ty2 coi2 =
	ACo $ FunTy (fromCoI coi1 ty1) (fromCoI coi2 ty2)

-- | Smart constructor for quantified 'Coercion's on 'CoercionI', see also 'mkForAllCoercion'
mkForAllTyCoI :: TyVar -> CoercionI -> CoercionI
mkForAllTyCoI _ IdCo = IdCo
mkForAllTyCoI tv (ACo co) = ACo $ ForAllTy tv co

-- | Extract a 'Coercion' from a 'CoercionI' if it represents one. If it is the identity coercion,
-- panic
fromACo :: CoercionI -> Coercion
fromACo (ACo co) = co

-- | Smart constructor for class 'Coercion's on 'CoercionI'. Satisfies:
--
-- > mkClassPPredCoI cls tys cois :: PredTy (cls tys) ~ PredTy (cls (tys `cast` cois))
mkClassPPredCoI :: Class -> [Type] -> [CoercionI] -> CoercionI
mkClassPPredCoI cls tys cois 
  | allIdCoIs cois = IdCo
  | otherwise      = ACo $ PredTy $ ClassP cls (zipCoArgs cois tys)

-- | Smart constructor for implicit parameter 'Coercion's on 'CoercionI'. Similar to 'mkClassPPredCoI'
mkIParamPredCoI :: (IPName Name) -> CoercionI -> CoercionI 
mkIParamPredCoI _   IdCo     = IdCo
mkIParamPredCoI ipn (ACo co) = ACo $ PredTy $ IParam ipn co

-- | Smart constructor for type equality 'Coercion's on 'CoercionI'. Similar to 'mkClassPPredCoI'
mkEqPredCoI :: Type -> CoercionI -> Type -> CoercionI -> CoercionI
mkEqPredCoI _    IdCo     _   IdCo      = IdCo
mkEqPredCoI ty1  IdCo     _   (ACo co2) = ACo $ PredTy $ EqPred ty1 co2
mkEqPredCoI _   (ACo co1) ty2 coi2      = ACo $ PredTy $ EqPred co1 (fromCoI coi2 ty2)
\end{code}

%************************************************************************
%*                                                                      *
                 Optimising coercions									
%*                                                                      *
%************************************************************************

\begin{code}
type NormalCo = Coercion
  -- Invariants: 
  --  * For trans coercions (co1 `trans` co2)
  --       co1 is not a trans, and neither co1 nor co2 is identity
  --  * If the coercion is the identity, it has no CoVars of CoTyCons in it (just types)

type NormalNonIdCo = NormalCo  -- Extra invariant: not the identity

optCoercion :: Coercion -> NormalCo
optCoercion co = opt_co False co

opt_co :: Bool	       -- True <=> return (sym co)
       -> Coercion
       -> NormalCo
opt_co = opt_co'
-- opt_co sym co = pprTrace "opt_co {" (ppr sym <+> ppr co) $
--       	        co1 `seq` 
--                pprTrace "opt_co done }" (ppr co1) 
--               WARN( not same_co_kind, ppr co  <+> dcolon <+> pprEqPred (s1,t1) 
--                                     $$ ppr co1 <+> dcolon <+> pprEqPred (s2,t2) )
--                co1
--  where
--    co1 = opt_co' sym co
--    same_co_kind = s1 `coreEqType` s2 && t1 `coreEqType` t2
--    (s,t) = coercionKind co
--    (s1,t1) | sym = (t,s)
--            | otherwise = (s,t)
--    (s2,t2) = coercionKind co1

opt_co' sym (AppTy ty1 ty2) 	      = mkAppTy (opt_co sym ty1) (opt_co sym ty2)
opt_co' sym (FunTy ty1 ty2) 	      = FunTy (opt_co sym ty1) (opt_co sym ty2)
opt_co' sym (PredTy (ClassP cls tys)) = PredTy (ClassP cls (map (opt_co sym) tys))
opt_co' sym (PredTy (IParam n ty))    = PredTy (IParam n (opt_co sym ty))

opt_co' sym co@(TyVarTy tv)
  | not (isCoVar tv)     = co   -- Identity; does not mention a CoVar
  | ty1 `coreEqType` ty2 = ty1	-- Identity; ..ditto..
  | not sym              = co
  | otherwise            = mkSymCoercion co
  where
    (ty1,ty2) = coVarKind tv

opt_co' sym (ForAllTy tv cor) 
  | isCoVar tv = mkCoPredTy (opt_co sym co1) (opt_co sym co2) (opt_co sym cor)
  | otherwise  = ForAllTy tv (opt_co sym cor)
  where
    (co1,co2) = coVarKind tv

opt_co' sym (TyConApp tc cos)
  | isCoercionTyCon tc
  = foldl mkAppTy opt_co_tc 
          (map (opt_co sym) (drop arity cos))
  | otherwise
  = TyConApp tc (map (opt_co sym) cos)
  where
    arity = tyConArity tc
    opt_co_tc :: NormalCo
    opt_co_tc = opt_co_tc_app sym tc (take arity cos)

--------
opt_co_tc_app :: Bool -> TyCon -> [Type] -> NormalCo
-- Used for CoercionTyCons only
opt_co_tc_app sym tc cos
  | tc `hasKey` symCoercionTyConKey
  = opt_co (not sym) co1

  | tc `hasKey` transCoercionTyConKey
  = if sym then opt_trans opt_co2 opt_co1
           else opt_trans opt_co1 opt_co2

  | tc `hasKey` leftCoercionTyConKey
  , Just (co1, _) <- splitAppTy_maybe opt_co1
  = co1

  | tc `hasKey` rightCoercionTyConKey
  , Just (_, co2) <- splitAppTy_maybe opt_co1
  = co2

  | tc `hasKey` csel1CoercionTyConKey
  , Just (s1,_,_) <- splitCoPredTy_maybe opt_co1
  = s1

  | tc `hasKey` csel2CoercionTyConKey
  , Just (_,s2,_) <- splitCoPredTy_maybe opt_co1
  = s2

  | tc `hasKey` cselRCoercionTyConKey
  , Just (_,_,r) <- splitCoPredTy_maybe opt_co1
  = r

  | tc `hasKey` instCoercionTyConKey
  , Just (tv, co'') <- splitForAllTy_maybe opt_co1
  , let ty = co2
  = substTyWith [tv] [ty] co''

  | otherwise	  -- Do not push sym inside top-level axioms
    		  -- e.g. if g is a top-level axiom
    		  --   g a : F a ~ a
		  -- Then (sym (g ty)) /= g (sym ty) !!
  = if sym then mkSymCoercion the_co 
           else the_co
  where
    the_co = TyConApp tc cos
    (co1 : cos1) = cos
    (co2 : _)    = cos1
    opt_co1 = opt_co sym co1
    opt_co2 = opt_co sym co2

-------------
opt_trans :: NormalCo -> NormalCo -> NormalCo
opt_trans co1 co2
  | isIdNormCo co1 = co2
  | otherwise      = opt_trans1 co1 co2

opt_trans1 :: NormalNonIdCo -> NormalCo -> NormalCo
-- First arg is not the identity
opt_trans1 co1 co2
  | isIdNormCo co2 = co1
  | otherwise      = opt_trans2 co1 co2

opt_trans2 :: NormalNonIdCo -> NormalNonIdCo -> NormalCo
-- Neither arg is the identity
opt_trans2 (TyConApp tc [co1a,co1b]) co2
  | tc `hasKey` transCoercionTyConKey
  = opt_trans1 co1a (opt_trans2 co1b co2)

opt_trans2 co1 co2 
  | Just co <- opt_trans_rule co1 co2
  = co

opt_trans2 co1 (TyConApp tc [co2a,co2b])
  | tc `hasKey` transCoercionTyConKey
  , Just co1_2a <- opt_trans_rule co1 co2a
  = if isIdNormCo co1_2a
    then co2b
    else opt_trans2 co1_2a co2b

opt_trans2 co1 co2
  = mkTransCoercion co1 co2

------
opt_trans_rule :: NormalNonIdCo -> NormalNonIdCo -> Maybe NormalCo
opt_trans_rule (TyConApp tc [co1]) co2
  | tc `hasKey` symCoercionTyConKey
  , co1 `coreEqType` co2
  , (_,ty2) <- coercionKind co2
  = Just ty2

opt_trans_rule co1 (TyConApp tc [co2])
  | tc `hasKey` symCoercionTyConKey
  , co1 `coreEqType` co2
  , (ty1,_) <- coercionKind co1
  = Just ty1

opt_trans_rule (TyConApp tc1 [co1,ty1]) (TyConApp tc2 [co2,ty2])
  | tc1 `hasKey` instCoercionTyConKey
  , tc1 == tc2
  , ty1 `coreEqType` ty2
  = Just (mkInstCoercion (opt_trans2 co1 co2) ty1) 

opt_trans_rule (TyConApp tc1 cos1) (TyConApp tc2 cos2)
  | not (isCoercionTyCon tc1) || 
    getUnique tc1 `elem` [ leftCoercionTyConKey, rightCoercionTyConKey
                         , csel1CoercionTyConKey, csel2CoercionTyConKey
 			 , cselRCoercionTyConKey ]	--Yuk!
  , tc1 == tc2 		 -- Works for left,right, and csel* family
    	   		 -- BUT NOT equality axioms 
			 -- E.g.        (g Int) `trans` (g Bool)
			 -- 	   /= g (Int . Bool)
  = Just (TyConApp tc1 (zipWith opt_trans cos1 cos2))

opt_trans_rule co1 co2
  | Just (co1a, co1b) <- splitAppTy_maybe co1
  , Just (co2a, co2b) <- splitAppTy_maybe co2
  = Just (mkAppTy (opt_trans co1a co2a) (opt_trans co1b co2b))

  | Just (s1,t1,r1) <- splitCoPredTy_maybe co1
  , Just (s2,t2,r2) <- splitCoPredTy_maybe co1
  = Just (mkCoPredTy (opt_trans s1 s2)
                     (opt_trans t1 t2)
                     (opt_trans r1 r2))

  | Just (tv1,r1) <- splitForAllTy_maybe co1
  , Just (tv2,r2) <- splitForAllTy_maybe co2
  , not (isCoVar tv1)		     -- Both have same kind
  , let r2' = substTyWith [tv2] [TyVarTy tv1] r2
  = Just (ForAllTy tv1 (opt_trans2 r1 r2'))

opt_trans_rule _ _ = Nothing

  
-------------
isIdNormCo :: NormalCo -> Bool
-- Cheap identity test: look for coercions with no coercion variables at all
-- So it'll return False for (sym g `trans` g)
isIdNormCo ty = go ty
  where
    go (TyVarTy tv)  	       = not (isCoVar tv)
    go (AppTy t1 t2) 	       = go t1 && go t2
    go (FunTy t1 t2) 	       = go t1 && go t2
    go (ForAllTy tv ty)        = go (tyVarKind tv) && go ty
    go (TyConApp tc tys)       = not (isCoercionTyCon tc) && all go tys
    go (PredTy (IParam _ ty))  = go ty
    go (PredTy (ClassP _ tys)) = all go tys
    go (PredTy (EqPred t1 t2)) = go t1 && go t2
\end{code}  
