{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Monadic type operations

This module contains monadic operations over types that contain
mutable type variables
-}

{-# LANGUAGE CPP, TupleSections, MultiWayIf #-}

module TcMType (
  TcTyVar, TcKind, TcType, TcTauType, TcThetaType, TcTyVarSet,

  --------------------------------
  -- Creating new mutable type variables
  newFlexiTyVar,
  newFlexiTyVarTy,              -- Kind -> TcM TcType
  newFlexiTyVarTys,             -- Int -> Kind -> TcM [TcType]
  newOpenFlexiTyVarTy, newOpenTypeKind,
  newMetaKindVar, newMetaKindVars, newMetaTyVarTyAtLevel,
  cloneMetaTyVar,
  newFmvTyVar, newFskTyVar,

  readMetaTyVar, writeMetaTyVar, writeMetaTyVarRef,
  newMetaDetails, isFilledMetaTyVar, isUnfilledMetaTyVar,

  --------------------------------
  -- Expected types
  ExpType(..), ExpSigmaType, ExpRhoType,
  mkCheckExpType,
  newInferExpType, newInferExpTypeInst, newInferExpTypeNoInst,
  readExpType, readExpType_maybe,
  expTypeToType, checkingExpType_maybe, checkingExpType,
  tauifyExpType, inferResultToType,

  --------------------------------
  -- Creating fresh type variables for pm checking
  genInstSkolTyVarsX,

  --------------------------------
  -- Creating new evidence variables
  newEvVar, newEvVars, newDict,
  newWanted, newWanteds, cloneWanted, cloneWC,
  emitWanted, emitWantedEq, emitWantedEvVar, emitWantedEvVars,
  newTcEvBinds, addTcEvBind,

  newCoercionHole, fillCoercionHole, isFilledCoercionHole,
  unpackCoercionHole, unpackCoercionHole_maybe,
  checkCoercionHole,

  --------------------------------
  -- Instantiation
  newMetaTyVars, newMetaTyVarX, newMetaTyVarsX,
  newMetaSigTyVars, newMetaSigTyVarX,
  newSigTyVar, newWildCardX,
  tcInstType,
  tcInstSkolTyVars,tcInstSkolTyVarsX,
  tcInstSuperSkolTyVarsX,
  tcSkolDFunType, tcSuperSkolTyVars,

  instSkolTyCoVars, freshenTyVarBndrs, freshenCoVarBndrsX,

  --------------------------------
  -- Zonking and tidying
  zonkTidyTcType, zonkTidyOrigin,
  tidyEvVar, tidyCt, tidySkolemInfo,
  skolemiseRuntimeUnk,
  zonkTcTyVar, zonkTcTyVars, zonkTcTyVarToTyVar,
  zonkTyCoVarsAndFV, zonkTcTypeAndFV,
  zonkTyCoVarsAndFVList,
  zonkTcTypeAndSplitDepVars, zonkTcTypesAndSplitDepVars,
  zonkQuantifiedTyVar, defaultTyVar,
  quantifyTyVars,
  zonkTcTyCoVarBndr, zonkTcTyVarBinder,
  zonkTcType, zonkTcTypes, zonkCo,
  zonkTyCoVarKind, zonkTcTypeMapper,

  zonkEvVar, zonkWC, zonkSimples, zonkId, zonkCt, zonkSkolemInfo,

  tcGetGlobalTyCoVars,

  ------------------------------
  -- Levity polymorphism
  ensureNotLevPoly, checkForLevPoly, checkForLevPolyX, formatLevPolyErr
  ) where

#include "HsVersions.h"

-- friends:
import TyCoRep
import TcType
import Type
import Kind
import Coercion
import Class
import Var

-- others:
import TcRnMonad        -- TcType, amongst others
import TcEvidence
import Id
import Name
import VarSet
import TysWiredIn
import TysPrim
import VarEnv
import NameEnv
import PrelNames
import Util
import Outputable
import FastString
import SrcLoc
import Bag
import Pair
import UniqSet
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Maybes
import Data.List        ( mapAccumL )
import Control.Arrow    ( second )

{-
************************************************************************
*                                                                      *
        Kind variables
*                                                                      *
************************************************************************
-}

mkKindName :: Unique -> Name
mkKindName unique = mkSystemName unique kind_var_occ

kind_var_occ :: OccName -- Just one for all MetaKindVars
                        -- They may be jiggled by tidying
kind_var_occ = mkOccName tvName "k"

newMetaKindVar :: TcM TcKind
newMetaKindVar = do { uniq <- newUnique
                    ; details <- newMetaDetails TauTv
                    ; let kv = mkTcTyVar (mkKindName uniq) liftedTypeKind details
                    ; return (mkTyVarTy kv) }

newMetaKindVars :: Int -> TcM [TcKind]
newMetaKindVars n = mapM (\ _ -> newMetaKindVar) (nOfThem n ())

{-
************************************************************************
*                                                                      *
     Evidence variables; range over constraints we can abstract over
*                                                                      *
************************************************************************
-}

newEvVars :: TcThetaType -> TcM [EvVar]
newEvVars theta = mapM newEvVar theta

--------------

newEvVar :: TcPredType -> TcRnIf gbl lcl EvVar
-- Creates new *rigid* variables for predicates
newEvVar ty = do { name <- newSysName (predTypeOccName ty)
                 ; return (mkLocalIdOrCoVar name ty) }

newWanted :: CtOrigin -> Maybe TypeOrKind -> PredType -> TcM CtEvidence
-- Deals with both equality and non-equality predicates
newWanted orig t_or_k pty
  = do loc <- getCtLocM orig t_or_k
       d <- if isEqPred pty then HoleDest  <$> newCoercionHole
                            else EvVarDest <$> newEvVar pty
       return $ CtWanted { ctev_dest = d
                         , ctev_pred = pty
                         , ctev_nosh = WDeriv
                         , ctev_loc = loc }

newWanteds :: CtOrigin -> ThetaType -> TcM [CtEvidence]
newWanteds orig = mapM (newWanted orig Nothing)

cloneWanted :: Ct -> TcM CtEvidence
cloneWanted ct
  = newWanted (ctEvOrigin ev) Nothing (ctEvPred ev)
  where
    ev = ctEvidence ct

cloneWC :: WantedConstraints -> TcM WantedConstraints
cloneWC wc@(WC { wc_simple = simples, wc_impl = implics })
  = do { simples' <- mapBagM clone_one simples
       ; implics' <- mapBagM clone_implic implics
       ; return (wc { wc_simple = simples', wc_impl = implics' }) }
  where
    clone_one ct = do { ev <- cloneWanted ct; return (mkNonCanonical ev) }

    clone_implic implic@(Implic { ic_wanted = inner_wanted })
      = do { inner_wanted' <- cloneWC inner_wanted
           ; return (implic { ic_wanted = inner_wanted' }) }

-- | Emits a new Wanted. Deals with both equalities and non-equalities.
emitWanted :: CtOrigin -> TcPredType -> TcM EvTerm
emitWanted origin pty
  = do { ev <- newWanted origin Nothing pty
       ; emitSimple $ mkNonCanonical ev
       ; return $ ctEvTerm ev }

-- | Emits a new equality constraint
emitWantedEq :: CtOrigin -> TypeOrKind -> Role -> TcType -> TcType -> TcM Coercion
emitWantedEq origin t_or_k role ty1 ty2
  = do { hole <- newCoercionHole
       ; loc <- getCtLocM origin (Just t_or_k)
       ; emitSimple $ mkNonCanonical $
         CtWanted { ctev_pred = pty, ctev_dest = HoleDest hole
                  , ctev_nosh = WDeriv, ctev_loc = loc }
       ; return (mkHoleCo hole role ty1 ty2) }
  where
    pty = mkPrimEqPredRole role ty1 ty2

-- | Creates a new EvVar and immediately emits it as a Wanted.
-- No equality predicates here.
emitWantedEvVar :: CtOrigin -> TcPredType -> TcM EvVar
emitWantedEvVar origin ty
  = do { new_cv <- newEvVar ty
       ; loc <- getCtLocM origin Nothing
       ; let ctev = CtWanted { ctev_dest = EvVarDest new_cv
                             , ctev_pred = ty
                             , ctev_nosh = WDeriv
                             , ctev_loc  = loc }
       ; emitSimple $ mkNonCanonical ctev
       ; return new_cv }

emitWantedEvVars :: CtOrigin -> [TcPredType] -> TcM [EvVar]
emitWantedEvVars orig = mapM (emitWantedEvVar orig)

newDict :: Class -> [TcType] -> TcM DictId
newDict cls tys
  = do { name <- newSysName (mkDictOcc (getOccName cls))
       ; return (mkLocalId name (mkClassPred cls tys)) }

predTypeOccName :: PredType -> OccName
predTypeOccName ty = case classifyPredType ty of
    ClassPred cls _ -> mkDictOcc (getOccName cls)
    EqPred _ _ _    -> mkVarOccFS (fsLit "cobox")
    IrredPred _     -> mkVarOccFS (fsLit "irred")

{-
************************************************************************
*                                                                      *
        Coercion holes
*                                                                      *
************************************************************************
-}

newCoercionHole :: TcM CoercionHole
newCoercionHole
  = do { u <- newUnique
       ; traceTc "New coercion hole:" (ppr u)
       ; ref <- newMutVar Nothing
       ; return $ CoercionHole u ref }

-- | Put a value in a coercion hole
fillCoercionHole :: CoercionHole -> Coercion -> TcM ()
fillCoercionHole (CoercionHole u ref) co
  = do {
#if defined(DEBUG)
       ; cts <- readTcRef ref
       ; whenIsJust cts $ \old_co ->
         pprPanic "Filling a filled coercion hole" (ppr u $$ ppr co $$ ppr old_co)
#endif
       ; traceTc "Filling coercion hole" (ppr u <+> text ":=" <+> ppr co)
       ; writeTcRef ref (Just co) }

-- | Is a coercion hole filled in?
isFilledCoercionHole :: CoercionHole -> TcM Bool
isFilledCoercionHole (CoercionHole _ ref) = isJust <$> readTcRef ref

-- | Retrieve the contents of a coercion hole. Panics if the hole
-- is unfilled
unpackCoercionHole :: CoercionHole -> TcM Coercion
unpackCoercionHole hole
  = do { contents <- unpackCoercionHole_maybe hole
       ; case contents of
           Just co -> return co
           Nothing -> pprPanic "Unfilled coercion hole" (ppr hole) }

-- | Retrieve the contents of a coercion hole, if it is filled
unpackCoercionHole_maybe :: CoercionHole -> TcM (Maybe Coercion)
unpackCoercionHole_maybe (CoercionHole _ ref) = readTcRef ref

-- | Check that a coercion is appropriate for filling a hole. (The hole
-- itself is needed only for printing. NB: This must be /lazy/ in the coercion,
-- as it's used in TcHsSyn in the presence of knots.
-- Always returns the checked coercion, but this return value is necessary
-- so that the input coercion is forced only when the output is forced.
checkCoercionHole :: Coercion -> CoercionHole -> Role -> Type -> Type -> TcM Coercion
checkCoercionHole co h r t1 t2
-- co is already zonked, but t1 and t2 might not be
  | debugIsOn
  = do { t1 <- zonkTcType t1
       ; t2 <- zonkTcType t2
       ; let (Pair _t1 _t2, _role) = coercionKindRole co
       ; return $
         ASSERT2( t1 `eqType` _t1 && t2 `eqType` _t2 && r == _role
                , (text "Bad coercion hole" <+>
                   ppr h <> colon <+> vcat [ ppr _t1, ppr _t2, ppr _role
                                           , ppr co, ppr t1, ppr t2
                                           , ppr r ]) )
         co }
  | otherwise
  = return co

{-
************************************************************************
*
    Expected types
*
************************************************************************

Note [ExpType]
~~~~~~~~~~~~~~

An ExpType is used as the "expected type" when type-checking an expression.
An ExpType can hold a "hole" that can be filled in by the type-checker.
This allows us to have one tcExpr that works in both checking mode and
synthesis mode (that is, bidirectional type-checking). Previously, this
was achieved by using ordinary unification variables, but we don't need
or want that generality. (For example, #11397 was caused by doing the
wrong thing with unification variables.) Instead, we observe that these
holes should

1. never be nested
2. never appear as the type of a variable
3. be used linearly (never be duplicated)

By defining ExpType, separately from Type, we can achieve goals 1 and 2
statically.

See also [wiki:Typechecking]

Note [TcLevel of ExpType]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  data G a where
    MkG :: G Bool

  foo MkG = True

This is a classic untouchable-variable / ambiguous GADT return type
scenario. But, with ExpTypes, we'll be inferring the type of the RHS.
And, because there is only one branch of the case, we won't trigger
Note [Case branches must never infer a non-tau type] of TcMatches.
We thus must track a TcLevel in an Inferring ExpType. If we try to
fill the ExpType and find that the TcLevels don't work out, we
fill the ExpType with a tau-tv at the low TcLevel, hopefully to
be worked out later by some means. This is triggered in
test gadt/gadt-escape1.

-}

-- actual data definition is in TcType

-- | Make an 'ExpType' suitable for inferring a type of kind * or #.
newInferExpTypeNoInst :: TcM ExpSigmaType
newInferExpTypeNoInst = newInferExpType False

newInferExpTypeInst :: TcM ExpRhoType
newInferExpTypeInst = newInferExpType True

newInferExpType :: Bool -> TcM ExpType
newInferExpType inst
  = do { u <- newUnique
       ; tclvl <- getTcLevel
       ; traceTc "newOpenInferExpType" (ppr u <+> ppr inst <+> ppr tclvl)
       ; ref <- newMutVar Nothing
       ; return (Infer (IR { ir_uniq = u, ir_lvl = tclvl
                           , ir_ref = ref, ir_inst = inst })) }

-- | Extract a type out of an ExpType, if one exists. But one should always
-- exist. Unless you're quite sure you know what you're doing.
readExpType_maybe :: ExpType -> TcM (Maybe TcType)
readExpType_maybe (Check ty)                   = return (Just ty)
readExpType_maybe (Infer (IR { ir_ref = ref})) = readMutVar ref

-- | Extract a type out of an ExpType. Otherwise, panics.
readExpType :: ExpType -> TcM TcType
readExpType exp_ty
  = do { mb_ty <- readExpType_maybe exp_ty
       ; case mb_ty of
           Just ty -> return ty
           Nothing -> pprPanic "Unknown expected type" (ppr exp_ty) }

-- | Returns the expected type when in checking mode.
checkingExpType_maybe :: ExpType -> Maybe TcType
checkingExpType_maybe (Check ty) = Just ty
checkingExpType_maybe _          = Nothing

-- | Returns the expected type when in checking mode. Panics if in inference
-- mode.
checkingExpType :: String -> ExpType -> TcType
checkingExpType _   (Check ty) = ty
checkingExpType err et         = pprPanic "checkingExpType" (text err $$ ppr et)

tauifyExpType :: ExpType -> TcM ExpType
-- ^ Turn a (Infer hole) type into a (Check alpha),
-- where alpha is a fresh unification variable
tauifyExpType (Check ty)      = return (Check ty)  -- No-op for (Check ty)
tauifyExpType (Infer inf_res) = do { ty <- inferResultToType inf_res
                                   ; return (Check ty) }

-- | Extracts the expected type if there is one, or generates a new
-- TauTv if there isn't.
expTypeToType :: ExpType -> TcM TcType
expTypeToType (Check ty)      = return ty
expTypeToType (Infer inf_res) = inferResultToType inf_res

inferResultToType :: InferResult -> TcM Type
inferResultToType (IR { ir_uniq = u, ir_lvl = tc_lvl
                      , ir_ref = ref })
  = do { rr  <- newMetaTyVarTyAtLevel tc_lvl runtimeRepTy
       ; tau <- newMetaTyVarTyAtLevel tc_lvl (tYPE rr)
             -- See Note [TcLevel of ExpType]
       ; writeMutVar ref (Just tau)
       ; traceTc "Forcing ExpType to be monomorphic:"
                 (ppr u <+> text ":=" <+> ppr tau)
       ; return tau }


{- *********************************************************************
*                                                                      *
        SkolemTvs (immutable)
*                                                                      *
********************************************************************* -}

tcInstType :: ([TyVar] -> TcM (TCvSubst, [TcTyVar]))
                   -- ^ How to instantiate the type variables
           -> Id                                            -- ^ Type to instantiate
           -> TcM ([(Name, TcTyVar)], TcThetaType, TcType)  -- ^ Result
                -- (type vars, preds (incl equalities), rho)
tcInstType inst_tyvars id
  = case tcSplitForAllTys (idType id) of
        ([],    rho) -> let     -- There may be overloading despite no type variables;
                                --      (?x :: Int) => Int -> Int
                                (theta, tau) = tcSplitPhiTy rho
                            in
                            return ([], theta, tau)

        (tyvars, rho) -> do { (subst, tyvars') <- inst_tyvars tyvars
                            ; let (theta, tau) = tcSplitPhiTy (substTyAddInScope subst rho)
                                  tv_prs       = map tyVarName tyvars `zip` tyvars'
                            ; return (tv_prs, theta, tau) }

tcSkolDFunType :: DFunId -> TcM ([TcTyVar], TcThetaType, TcType)
-- Instantiate a type signature with skolem constants.
-- We could give them fresh names, but no need to do so
tcSkolDFunType dfun
  = do { (tv_prs, theta, tau) <- tcInstType tcInstSuperSkolTyVars dfun
       ; return (map snd tv_prs, theta, tau) }

tcSuperSkolTyVars :: [TyVar] -> (TCvSubst, [TcTyVar])
-- Make skolem constants, but do *not* give them new names, as above
-- Moreover, make them "super skolems"; see comments with superSkolemTv
-- see Note [Kind substitution when instantiating]
-- Precondition: tyvars should be ordered by scoping
tcSuperSkolTyVars = mapAccumL tcSuperSkolTyVar emptyTCvSubst

tcSuperSkolTyVar :: TCvSubst -> TyVar -> (TCvSubst, TcTyVar)
tcSuperSkolTyVar subst tv
  = (extendTvSubstWithClone subst tv new_tv, new_tv)
  where
    kind   = substTyUnchecked subst (tyVarKind tv)
    new_tv = mkTcTyVar (tyVarName tv) kind superSkolemTv

-- | Given a list of @['TyVar']@, skolemize the type variables,
-- returning a substitution mapping the original tyvars to the
-- skolems, and the list of newly bound skolems.  See also
-- tcInstSkolTyVars' for a precondition.  The resulting
-- skolems are non-overlappable; see Note [Overlap and deriving]
-- for an example where this matters.
tcInstSkolTyVars :: [TyVar] -> TcM (TCvSubst, [TcTyVar])
tcInstSkolTyVars = tcInstSkolTyVarsX emptyTCvSubst

tcInstSkolTyVarsX :: TCvSubst -> [TyVar] -> TcM (TCvSubst, [TcTyVar])
tcInstSkolTyVarsX = tcInstSkolTyVars' False

tcInstSuperSkolTyVars :: [TyVar] -> TcM (TCvSubst, [TcTyVar])
tcInstSuperSkolTyVars = tcInstSuperSkolTyVarsX emptyTCvSubst

tcInstSuperSkolTyVarsX :: TCvSubst -> [TyVar] -> TcM (TCvSubst, [TcTyVar])
tcInstSuperSkolTyVarsX subst = tcInstSkolTyVars' True subst

tcInstSkolTyVars' :: Bool -> TCvSubst -> [TyVar] -> TcM (TCvSubst, [TcTyVar])
-- Precondition: tyvars should be ordered (kind vars first)
-- see Note [Kind substitution when instantiating]
-- Get the location from the monad; this is a complete freshening operation
tcInstSkolTyVars' overlappable subst tvs
  = do { loc <- getSrcSpanM
       ; lvl <- getTcLevel
       ; instSkolTyCoVarsX (mkTcSkolTyVar lvl loc overlappable) subst tvs }

mkTcSkolTyVar :: TcLevel -> SrcSpan -> Bool -> TcTyVarMaker
mkTcSkolTyVar lvl loc overlappable
  = \ uniq old_name kind -> mkTcTyVar (mkInternalName uniq (getOccName old_name) loc)
                                      kind details
  where
    details = SkolemTv (pushTcLevel lvl) overlappable
              -- NB: skolems bump the level

------------------
freshenTyVarBndrs :: [TyVar] -> TcRnIf gbl lcl (TCvSubst, [TyVar])
-- ^ Give fresh uniques to a bunch of TyVars, but they stay
--   as TyVars, rather than becoming TcTyVars
-- Used in FamInst.newFamInst, and Inst.newClsInst
freshenTyVarBndrs = instSkolTyCoVars mk_tv
  where
    mk_tv uniq old_name kind = mkTyVar (setNameUnique old_name uniq) kind

freshenCoVarBndrsX :: TCvSubst -> [CoVar] -> TcRnIf gbl lcl (TCvSubst, [CoVar])
-- ^ Give fresh uniques to a bunch of CoVars
-- Used in FamInst.newFamInst
freshenCoVarBndrsX subst = instSkolTyCoVarsX mk_cv subst
  where
    mk_cv uniq old_name kind = mkCoVar (setNameUnique old_name uniq) kind

------------------
type TcTyVarMaker = Unique -> Name -> Kind -> TyCoVar
instSkolTyCoVars :: TcTyVarMaker -> [TyVar] -> TcRnIf gbl lcl (TCvSubst, [TyCoVar])
instSkolTyCoVars mk_tcv = instSkolTyCoVarsX mk_tcv emptyTCvSubst

instSkolTyCoVarsX :: TcTyVarMaker
                  -> TCvSubst -> [TyCoVar] -> TcRnIf gbl lcl (TCvSubst, [TyCoVar])
instSkolTyCoVarsX mk_tcv = mapAccumLM (instSkolTyCoVarX mk_tcv)

instSkolTyCoVarX :: TcTyVarMaker
                 -> TCvSubst -> TyCoVar -> TcRnIf gbl lcl (TCvSubst, TyCoVar)
instSkolTyCoVarX mk_tcv subst tycovar
  = do  { uniq <- newUnique  -- using a new unique is critical. See
                             -- Note [Skolems in zonkSyntaxExpr] in TcHsSyn
        ; let new_tcv = mk_tcv uniq old_name kind
              subst1 | isTyVar new_tcv
                     = extendTvSubstWithClone subst tycovar new_tcv
                     | otherwise
                     = extendCvSubstWithClone subst tycovar new_tcv
        ; return (subst1, new_tcv) }
  where
    old_name = tyVarName tycovar
    kind     = substTyUnchecked subst (tyVarKind tycovar)

newFskTyVar :: TcType -> TcM TcTyVar
newFskTyVar fam_ty
  = do { uniq <- newUnique
       ; ref  <- newMutVar Flexi
       ; let details = MetaTv { mtv_info  = FlatSkolTv
                              , mtv_ref   = ref
                              , mtv_tclvl = fmvTcLevel }
             name = mkMetaTyVarName uniq (fsLit "fsk")
       ; return (mkTcTyVar name (typeKind fam_ty) details) }

{-
Note [Kind substitution when instantiating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we instantiate a bunch of kind and type variables, first we
expect them to be topologically sorted.
Then we have to instantiate the kind variables, build a substitution
from old variables to the new variables, then instantiate the type
variables substituting the original kind.

Exemple: If we want to instantiate
  [(k1 :: *), (k2 :: *), (a :: k1 -> k2), (b :: k1)]
we want
  [(?k1 :: *), (?k2 :: *), (?a :: ?k1 -> ?k2), (?b :: ?k1)]
instead of the buggous
  [(?k1 :: *), (?k2 :: *), (?a :: k1 -> k2), (?b :: k1)]


************************************************************************
*                                                                      *
        MetaTvs (meta type variables; mutable)
*                                                                      *
************************************************************************
-}

mkMetaTyVarName :: Unique -> FastString -> Name
-- Makes a /System/ Name, which is eagerly eliminated by
-- the unifier; see TcUnify.nicer_to_update_tv1, and
-- TcCanonical.canEqTyVarTyVar (nicer_to_update_tv2)
mkMetaTyVarName uniq str = mkSysTvName uniq str

newSigTyVar :: Name -> Kind -> TcM TcTyVar
newSigTyVar name kind
  = do { details <- newMetaDetails SigTv
       ; return (mkTcTyVar name kind details) }

newFmvTyVar :: TcType -> TcM TcTyVar
-- Very like newMetaTyVar, except sets mtv_tclvl to one less
-- so that the fmv is untouchable.
newFmvTyVar fam_ty
  = do { uniq <- newUnique
       ; ref  <- newMutVar Flexi
       ; let details = MetaTv { mtv_info  = FlatMetaTv
                              , mtv_ref   = ref
                              , mtv_tclvl = fmvTcLevel }
             name = mkMetaTyVarName uniq (fsLit "s")
       ; return (mkTcTyVar name (typeKind fam_ty) details) }

newMetaDetails :: MetaInfo -> TcM TcTyVarDetails
newMetaDetails info
  = do { ref <- newMutVar Flexi
       ; tclvl <- getTcLevel
       ; return (MetaTv { mtv_info = info
                        , mtv_ref = ref
                        , mtv_tclvl = tclvl }) }

cloneMetaTyVar :: TcTyVar -> TcM TcTyVar
cloneMetaTyVar tv
  = ASSERT( isTcTyVar tv )
    do  { uniq <- newUnique
        ; ref  <- newMutVar Flexi
        ; let name'    = setNameUnique (tyVarName tv) uniq
              details' = case tcTyVarDetails tv of
                           details@(MetaTv {}) -> details { mtv_ref = ref }
                           _ -> pprPanic "cloneMetaTyVar" (ppr tv)
        ; return (mkTcTyVar name' (tyVarKind tv) details') }

-- Works for both type and kind variables
readMetaTyVar :: TyVar -> TcM MetaDetails
readMetaTyVar tyvar = ASSERT2( isMetaTyVar tyvar, ppr tyvar )
                      readMutVar (metaTyVarRef tyvar)

isFilledMetaTyVar :: TyVar -> TcM Bool
-- True of a filled-in (Indirect) meta type variable
isFilledMetaTyVar tv
  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tv
  = do  { details <- readMutVar ref
        ; return (isIndirect details) }
  | otherwise = return False

isUnfilledMetaTyVar :: TyVar -> TcM Bool
-- True of a un-filled-in (Flexi) meta type variable
isUnfilledMetaTyVar tv
  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tv
  = do  { details <- readMutVar ref
        ; return (isFlexi details) }
  | otherwise = return False

--------------------
-- Works with both type and kind variables
writeMetaTyVar :: TcTyVar -> TcType -> TcM ()
-- Write into a currently-empty MetaTyVar

writeMetaTyVar tyvar ty
  | not debugIsOn
  = writeMetaTyVarRef tyvar (metaTyVarRef tyvar) ty

-- Everything from here on only happens if DEBUG is on
  | not (isTcTyVar tyvar)
  = WARN( True, text "Writing to non-tc tyvar" <+> ppr tyvar )
    return ()

  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tyvar
  = writeMetaTyVarRef tyvar ref ty

  | otherwise
  = WARN( True, text "Writing to non-meta tyvar" <+> ppr tyvar )
    return ()

--------------------
writeMetaTyVarRef :: TcTyVar -> TcRef MetaDetails -> TcType -> TcM ()
-- Here the tyvar is for error checking only;
-- the ref cell must be for the same tyvar
writeMetaTyVarRef tyvar ref ty
  | not debugIsOn
  = do { traceTc "writeMetaTyVar" (ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)
                                   <+> text ":=" <+> ppr ty)
       ; writeTcRef ref (Indirect ty) }

  -- Everything from here on only happens if DEBUG is on
  | otherwise
  = do { meta_details <- readMutVar ref;
       -- Zonk kinds to allow the error check to work
       ; zonked_tv_kind <- zonkTcType tv_kind
       ; zonked_ty_kind <- zonkTcType ty_kind
       ; let kind_check_ok = isPredTy tv_kind  -- Don't check kinds for updates
                                               -- to coercion variables
                          || tcEqKind zonked_ty_kind zonked_tv_kind

             kind_msg = hang (text "Ill-kinded update to meta tyvar")
                           2 (    ppr tyvar <+> text "::" <+> (ppr tv_kind $$ ppr zonked_tv_kind)
                              <+> text ":="
                              <+> ppr ty <+> text "::" <+> (ppr ty_kind $$ ppr zonked_ty_kind) )

       ; traceTc "writeMetaTyVar" (ppr tyvar <+> text ":=" <+> ppr ty)

       -- Check for double updates
       ; MASSERT2( isFlexi meta_details, double_upd_msg meta_details )

       -- Check for level OK
       -- See Note [Level check when unifying]
       ; MASSERT2( level_check_ok, level_check_msg )

       -- Check Kinds ok
       ; MASSERT2( kind_check_ok, kind_msg )

       -- Do the write
       ; writeMutVar ref (Indirect ty) }
  where
    tv_kind = tyVarKind tyvar
    ty_kind = typeKind ty

    tv_lvl = tcTyVarLevel tyvar
    ty_lvl = tcTypeLevel ty

    level_check_ok = isFlattenTyVar tyvar
                  || not (ty_lvl `strictlyDeeperThan` tv_lvl)
    level_check_msg = ppr ty_lvl $$ ppr tv_lvl $$ ppr tyvar $$ ppr ty

    double_upd_msg details = hang (text "Double update of meta tyvar")
                                2 (ppr tyvar $$ ppr details)


{- Note [Level check when unifying]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When unifying
     alpha:lvl := ty
we expect that the TcLevel of 'ty' will be <= lvl.
However, during unflatting we do
     fuv:l := ty:(l+1)
which is usually wrong; hence the check isFmmvTyVar in level_check_ok.
See Note [TcLevel assignment] in TcType.
-}

{-
% Generating fresh variables for pattern match check
-}

-- UNINSTANTIATED VERSION OF tcInstSkolTyCoVars
genInstSkolTyVarsX :: SrcSpan -> TCvSubst -> [TyVar]
                   -> TcRnIf gbl lcl (TCvSubst, [TcTyVar])
-- Precondition: tyvars should be scoping-ordered
-- see Note [Kind substitution when instantiating]
-- Get the location from the monad; this is a complete freshening operation
genInstSkolTyVarsX loc subst tvs
  = instSkolTyCoVarsX (mkTcSkolTyVar topTcLevel loc False) subst tvs

{-
************************************************************************
*                                                                      *
        MetaTvs: TauTvs
*                                                                      *
************************************************************************

Note [Never need to instantiate coercion variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With coercion variables sloshing around in types, it might seem that we
sometimes need to instantiate coercion variables. This would be problematic,
because coercion variables inhabit unboxed equality (~#), and the constraint
solver thinks in terms only of boxed equality (~). The solution is that
we never need to instantiate coercion variables in the first place.

The tyvars that we need to instantiate come from the types of functions,
data constructors, and patterns. These will never be quantified over
coercion variables, except for the special case of the promoted Eq#. But,
that can't ever appear in user code, so we're safe!
-}

newAnonMetaTyVar :: MetaInfo -> Kind -> TcM TcTyVar
-- Make a new meta tyvar out of thin air
newAnonMetaTyVar meta_info kind
  = do  { uniq <- newUnique
        ; let name = mkMetaTyVarName uniq s
              s = case meta_info of
                        TauTv       -> fsLit "t"
                        FlatMetaTv  -> fsLit "fmv"
                        FlatSkolTv  -> fsLit "fsk"
                        SigTv       -> fsLit "a"
        ; details <- newMetaDetails meta_info
        ; return (mkTcTyVar name kind details) }

newFlexiTyVar :: Kind -> TcM TcTyVar
newFlexiTyVar kind = newAnonMetaTyVar TauTv kind

newFlexiTyVarTy :: Kind -> TcM TcType
newFlexiTyVarTy kind = do
    tc_tyvar <- newFlexiTyVar kind
    return (mkTyVarTy tc_tyvar)

newFlexiTyVarTys :: Int -> Kind -> TcM [TcType]
newFlexiTyVarTys n kind = mapM newFlexiTyVarTy (nOfThem n kind)

newOpenTypeKind :: TcM TcKind
newOpenTypeKind
  = do { rr <- newFlexiTyVarTy runtimeRepTy
       ; return (tYPE rr) }

-- | Create a tyvar that can be a lifted or unlifted type.
-- Returns alpha :: TYPE kappa, where both alpha and kappa are fresh
newOpenFlexiTyVarTy :: TcM TcType
newOpenFlexiTyVarTy
  = do { kind <- newOpenTypeKind
       ; newFlexiTyVarTy kind }

newMetaSigTyVars :: [TyVar] -> TcM (TCvSubst, [TcTyVar])
newMetaSigTyVars = mapAccumLM newMetaSigTyVarX emptyTCvSubst

newMetaTyVars :: [TyVar] -> TcM (TCvSubst, [TcTyVar])
-- Instantiate with META type variables
-- Note that this works for a sequence of kind, type, and coercion variables
-- variables.  Eg    [ (k:*), (a:k->k) ]
--             Gives [ (k7:*), (a8:k7->k7) ]
newMetaTyVars = mapAccumLM newMetaTyVarX emptyTCvSubst
    -- emptyTCvSubst has an empty in-scope set, but that's fine here
    -- Since the tyvars are freshly made, they cannot possibly be
    -- captured by any existing for-alls.

newMetaTyVarX :: TCvSubst -> TyVar -> TcM (TCvSubst, TcTyVar)
-- Make a new unification variable tyvar whose Name and Kind come from
-- an existing TyVar. We substitute kind variables in the kind.
newMetaTyVarX subst tyvar = new_meta_tv_x TauTv subst tyvar

newMetaTyVarsX :: TCvSubst -> [TyVar] -> TcM (TCvSubst, [TcTyVar])
-- Just like newMetaTyVars, but start with an existing substitution.
newMetaTyVarsX subst = mapAccumLM newMetaTyVarX subst

newMetaSigTyVarX :: TCvSubst -> TyVar -> TcM (TCvSubst, TcTyVar)
-- Just like newMetaTyVarX, but make a SigTv
newMetaSigTyVarX subst tyvar = new_meta_tv_x SigTv subst tyvar

newWildCardX :: TCvSubst -> TyVar -> TcM (TCvSubst, TcTyVar)
newWildCardX subst tv
  = do { new_tv <- newAnonMetaTyVar TauTv (substTy subst (tyVarKind tv))
       ; return (extendTvSubstWithClone subst tv new_tv, new_tv) }

new_meta_tv_x :: MetaInfo -> TCvSubst -> TyVar -> TcM (TCvSubst, TcTyVar)
new_meta_tv_x info subst tv
  = do  { uniq <- newUnique
        ; details <- newMetaDetails info
        ; let name   = mkSystemName uniq (getOccName tv)
                       -- See Note [Name of an instantiated type variable]
              kind   = substTyUnchecked subst (tyVarKind tv)
                       -- NOTE: Trac #12549 is fixed so we could use
                       -- substTy here, but the tc_infer_args problem
                       -- is not yet fixed so leaving as unchecked for now.
                       -- OLD NOTE:
                       -- Unchecked because we call newMetaTyVarX from
                       -- tcInstBinder, which is called from tc_infer_args
                       -- which does not yet take enough trouble to ensure
                       -- the in-scope set is right; e.g. Trac #12785 trips
                       -- if we use substTy here
              new_tv = mkTcTyVar name kind details
              subst1 = extendTvSubstWithClone subst tv new_tv
        ; return (subst1, new_tv) }

newMetaTyVarTyAtLevel :: TcLevel -> TcKind -> TcM TcType
newMetaTyVarTyAtLevel tc_lvl kind
  = do  { uniq <- newUnique
        ; ref  <- newMutVar Flexi
        ; let name = mkMetaTyVarName uniq (fsLit "p")
              details = MetaTv { mtv_info  = TauTv
                               , mtv_ref   = ref
                               , mtv_tclvl = tc_lvl }
        ; return (mkTyVarTy (mkTcTyVar name kind details)) }

{- Note [Name of an instantiated type variable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At the moment we give a unification variable a System Name, which
influences the way it is tidied; see TypeRep.tidyTyVarBndr.

************************************************************************
*                                                                      *
             Quantification
*                                                                      *
************************************************************************

Note [quantifyTyVars]
~~~~~~~~~~~~~~~~~~~~~
quantifyTyVars is given the free vars of a type that we
are about to wrap in a forall.

It takes these free type/kind variables (partitioned into dependent and
non-dependent variables) and
  1. Zonks them and remove globals and covars
  2. Extends kvs1 with free kind vars in the kinds of tvs (removing globals)
  3. Calls zonkQuantifiedTyVar on each

Step (2) is often unimportant, because the kind variable is often
also free in the type.  Eg
     Typeable k (a::k)
has free vars {k,a}.  But the type (see Trac #7916)
    (f::k->*) (a::k)
has free vars {f,a}, but we must add 'k' as well! Hence step (3).

* This function distinguishes between dependent and non-dependent
  variables only to keep correct defaulting behavior with -XNoPolyKinds.
  With -XPolyKinds, it treats both classes of variables identically.

* quantifyTyVars never quantifies over
    - a coercion variable
    - a runtime-rep variable

Note [quantifyTyVars determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The results of quantifyTyVars are wrapped in a forall and can end up in the
interface file. One such example is inferred type signatures. They also affect
the results of optimizations, for example worker-wrapper. This means that to
get deterministic builds quantifyTyVars needs to be deterministic.

To achieve this CandidatesQTvs is backed by deterministic sets which allows them
to be later converted to a list in a deterministic order.

For more information about deterministic sets see
Note [Deterministic UniqFM] in UniqDFM.
-}

quantifyTyVars
  :: TcTyCoVarSet     -- Global tvs; already zonked
  -> CandidatesQTvs   -- See Note [Dependent type variables] in TcType
                      -- Already zonked
  -> TcM [TcTyVar]
-- See Note [quantifyTyVars]
-- Can be given a mixture of TcTyVars and TyVars, in the case of
--   associated type declarations. Also accepts covars, but *never* returns any.

quantifyTyVars gbl_tvs dvs@(DV{ dv_kvs = dep_tkvs, dv_tvs = nondep_tkvs })
  = do { traceTc "quantifyTyVars" (vcat [ppr dvs, ppr gbl_tvs])
       ; let all_cvs = filterVarSet isCoVar $ dVarSetToVarSet dep_tkvs
             dep_kvs = dVarSetElemsWellScoped $
                       dep_tkvs `dVarSetMinusVarSet` gbl_tvs
                                `dVarSetMinusVarSet` closeOverKinds all_cvs
                 -- dVarSetElemsWellScoped: put the kind variables into
                 --    well-scoped order.
                 --    E.g.  [k, (a::k)] not the other way roud
                 -- closeOverKinds all_cvs: do not quantify over coercion
                 --    variables, or any any tvs that a covar depends on

             nondep_tvs = dVarSetElems $
                          (nondep_tkvs `minusDVarSet` dep_tkvs)
                           `dVarSetMinusVarSet` gbl_tvs
                 -- See Note [Dependent type variables] in TcType
                 -- The `minus` dep_tkvs removes any kind-level vars
                 --    e.g. T k (a::k)   Since k appear in a kind it'll
                 --    be in dv_kvs, and is dependent. So remove it from
                 --    dv_tvs which will also contain k
                 -- No worry about dependent covars here;
                 --    they are all in dep_tkvs
                 -- No worry about scoping, because these are all
                 --    type variables
                 -- NB kinds of tvs are zonked by zonkTyCoVarsAndFV

             -- In the non-PolyKinds case, default the kind variables
             -- to *, and zonk the tyvars as usual.  Notice that this
             -- may make quantifyTyVars return a shorter list
             -- than it was passed, but that's ok
       ; poly_kinds  <- xoptM LangExt.PolyKinds
       ; dep_kvs'    <- mapMaybeM (zonk_quant (not poly_kinds)) dep_kvs
       ; nondep_tvs' <- mapMaybeM (zonk_quant False)            nondep_tvs
           -- Because of the order, any kind variables
           -- mentioned in the kinds of the nondep_tvs'
           -- now refer to the dep_kvs'

       ; traceTc "quantifyTyVars"
           (vcat [ text "globals:" <+> ppr gbl_tvs
                 , text "nondep:"  <+> pprTyVars nondep_tvs
                 , text "dep:"     <+> pprTyVars dep_kvs
                 , text "dep_kvs'" <+> pprTyVars dep_kvs'
                 , text "nondep_tvs'" <+> pprTyVars nondep_tvs' ])

       ; return (dep_kvs' ++ nondep_tvs') }
  where
    -- zonk_quant returns a tyvar if it should be quantified over;
    -- otherwise, it returns Nothing. The latter case happens for
    --    * Kind variables, with -XNoPolyKinds: don't quantify over these
    --    * RuntimeRep variables: we never quantify over these
    zonk_quant default_kind tkv
      | not (isTcTyVar tkv)
      = return (Just tkv)  -- For associated types, we have the class variables
                           -- in scope, and they are TyVars not TcTyVars
      | otherwise
      = do { deflt_done <- defaultTyVar default_kind tkv
           ; case deflt_done of
               True  -> return Nothing
               False -> do { tv <- zonkQuantifiedTyVar tkv
                           ; return (Just tv) } }

zonkQuantifiedTyVar :: TcTyVar -> TcM TcTyVar
-- The quantified type variables often include meta type variables
-- we want to freeze them into ordinary type variables
-- The meta tyvar is updated to point to the new skolem TyVar.  Now any
-- bound occurrences of the original type variable will get zonked to
-- the immutable version.
--
-- We leave skolem TyVars alone; they are immutable.
--
-- This function is called on both kind and type variables,
-- but kind variables *only* if PolyKinds is on.

zonkQuantifiedTyVar tv
  = case tcTyVarDetails tv of
      SkolemTv {} -> do { kind <- zonkTcType (tyVarKind tv)
                        ; return (setTyVarKind tv kind) }
        -- It might be a skolem type variable,
        -- for example from a user type signature

      MetaTv {} -> skolemiseUnboundMetaTyVar tv

      _other -> pprPanic "zonkQuantifiedTyVar" (ppr tv) -- RuntimeUnk

defaultTyVar :: Bool      -- True <=> please default this kind variable to *
             -> TcTyVar   -- If it's a MetaTyVar then it is unbound
             -> TcM Bool  -- True <=> defaulted away altogether

defaultTyVar default_kind tv
  | not (isMetaTyVar tv)
  = return False

  | isRuntimeRepVar tv && not_sig_tv  -- We never quantify over a RuntimeRep var
  = do { traceTc "Defaulting a RuntimeRep var to LiftedRep" (ppr tv)
       ; writeMetaTyVar tv liftedRepTy
       ; return True }

  | default_kind && not_sig_tv        -- -XNoPolyKinds and this is a kind var
  = do { default_kind_var tv          -- so default it to * if possible
       ; return True }

  | otherwise
  = return False

  where
    -- Do not default SigTvs. Doing so would violate the invariants
    -- on SigTvs; see Note [Signature skolems] in TcType.
    -- Trac #13343 is an example
    not_sig_tv = not (isSigTyVar tv)

    default_kind_var :: TyVar -> TcM ()
       -- defaultKindVar is used exclusively with -XNoPolyKinds
       -- See Note [Defaulting with -XNoPolyKinds]
       -- It takes an (unconstrained) meta tyvar and defaults it.
       -- Works only on vars of type *; for other kinds, it issues an error.
    default_kind_var kv
      | isStarKind (tyVarKind kv)
      = do { traceTc "Defaulting a kind var to *" (ppr kv)
           ; writeMetaTyVar kv liftedTypeKind }
      | otherwise
      = addErr (vcat [ text "Cannot default kind variable" <+> quotes (ppr kv')
                     , text "of kind:" <+> ppr (tyVarKind kv')
                     , text "Perhaps enable PolyKinds or add a kind signature" ])
      where
        (_, kv') = tidyOpenTyCoVar emptyTidyEnv kv

skolemiseRuntimeUnk :: TcTyVar -> TcM TyVar
skolemiseRuntimeUnk tv
  = skolemise_tv tv RuntimeUnk

skolemiseUnboundMetaTyVar :: TcTyVar -> TcM TyVar
skolemiseUnboundMetaTyVar tv
  = skolemise_tv tv (SkolemTv (metaTyVarTcLevel tv) False)

skolemise_tv :: TcTyVar -> TcTyVarDetails -> TcM TyVar
-- We have a Meta tyvar with a ref-cell inside it
-- Skolemise it, so that
--   we are totally out of Meta-tyvar-land
-- We create a skolem TyVar, not a regular TyVar
--   See Note [Zonking to Skolem]
skolemise_tv tv details
  = ASSERT2( isMetaTyVar tv, ppr tv )
    do  { when debugIsOn (check_empty tv)
        ; span <- getSrcSpanM    -- Get the location from "here"
                                 -- ie where we are generalising
        ; kind <- zonkTcType (tyVarKind tv)
        ; let uniq        = getUnique tv
                -- NB: Use same Unique as original tyvar. This is
                -- important for TcHsType.splitTelescopeTvs to work properly

              tv_name     = getOccName tv
              final_name  = mkInternalName uniq tv_name span
              final_tv    = mkTcTyVar final_name kind details

        ; traceTc "Skolemising" (ppr tv <+> text ":=" <+> ppr final_tv)
        ; writeMetaTyVar tv (mkTyVarTy final_tv)
        ; return final_tv }

  where
    check_empty tv       -- [Sept 04] Check for non-empty.
      = when debugIsOn $  -- See note [Silly Type Synonym]
        do { cts <- readMetaTyVar tv
           ; case cts of
               Flexi       -> return ()
               Indirect ty -> WARN( True, ppr tv $$ ppr ty )
                              return () }

{- Note [Defaulting with -XNoPolyKinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  data Compose f g a = Mk (f (g a))

We infer

  Compose :: forall k1 k2. (k2 -> *) -> (k1 -> k2) -> k1 -> *
  Mk :: forall k1 k2 (f :: k2 -> *) (g :: k1 -> k2) (a :: k1).
        f (g a) -> Compose k1 k2 f g a

Now, in another module, we have -XNoPolyKinds -XDataKinds in effect.
What does 'Mk mean? Pre GHC-8.0 with -XNoPolyKinds,
we just defaulted all kind variables to *. But that's no good here,
because the kind variables in 'Mk aren't of kind *, so defaulting to *
is ill-kinded.

After some debate on #11334, we decided to issue an error in this case.
The code is in defaultKindVar.

Note [What is a meta variable?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A "meta type-variable", also know as a "unification variable" is a placeholder
introduced by the typechecker for an as-yet-unknown monotype.

For example, when we see a call `reverse (f xs)`, we know that we calling
    reverse :: forall a. [a] -> [a]
So we know that the argument `f xs` must be a "list of something". But what is
the "something"? We don't know until we explore the `f xs` a bit more. So we set
out what we do know at the call of `reverse` by instantiate its type with a fresh
meta tyvar, `alpha` say. So now the type of the argument `f xs`, and of the
result, is `[alpha]`. The unification variable `alpha` stands for the
as-yet-unknown type of the elements of the list.

As type inference progresses we may learn more about `alpha`. For example, suppose
`f` has the type
    f :: forall b. b -> [Maybe b]
Then we instantiate `f`'s type with another fresh unification variable, say
`beta`; and equate `f`'s result type with reverse's argument type, thus
`[alpha] ~ [Maybe beta]`.

Now we can solve this equality to learn that `alpha ~ Maybe beta`, so we've
refined our knowledge about `alpha`. And so on.

If you found this Note useful, you may also want to have a look at
Section 5 of "Practical type inference for higher rank types" (Peyton Jones,
Vytiniotis, Weirich and Shields. J. Functional Programming. 2011).

Note [What is zonking?]
~~~~~~~~~~~~~~~~~~~~~~~
GHC relies heavily on mutability in the typechecker for efficient operation.
For this reason, throughout much of the type checking process meta type
variables (the MetaTv constructor of TcTyVarDetails) are represented by mutable
variables (known as TcRefs).

Zonking is the process of ripping out these mutable variables and replacing them
with a real Type. This involves traversing the entire type expression, but the
interesting part of replacing the mutable variables occurs in zonkTyVarOcc.

There are two ways to zonk a Type:

 * zonkTcTypeToType, which is intended to be used at the end of type-checking
   for the final zonk. It has to deal with unfilled metavars, either by filling
   it with a value like Any or failing (determined by the UnboundTyVarZonker
   used).

 * zonkTcType, which will happily ignore unfilled metavars. This is the
   appropriate function to use while in the middle of type-checking.

Note [Zonking to Skolem]
~~~~~~~~~~~~~~~~~~~~~~~~
We used to zonk quantified type variables to regular TyVars.  However, this
leads to problems.  Consider this program from the regression test suite:

  eval :: Int -> String -> String -> String
  eval 0 root actual = evalRHS 0 root actual

  evalRHS :: Int -> a
  evalRHS 0 root actual = eval 0 root actual

It leads to the deferral of an equality (wrapped in an implication constraint)

  forall a. () => ((String -> String -> String) ~ a)

which is propagated up to the toplevel (see TcSimplify.tcSimplifyInferCheck).
In the meantime `a' is zonked and quantified to form `evalRHS's signature.
This has the *side effect* of also zonking the `a' in the deferred equality
(which at this point is being handed around wrapped in an implication
constraint).

Finally, the equality (with the zonked `a') will be handed back to the
simplifier by TcRnDriver.tcRnSrcDecls calling TcSimplify.tcSimplifyTop.
If we zonk `a' with a regular type variable, we will have this regular type
variable now floating around in the simplifier, which in many places assumes to
only see proper TcTyVars.

We can avoid this problem by zonking with a skolem.  The skolem is rigid
(which we require for a quantified variable), but is still a TcTyVar that the
simplifier knows how to deal with.

Note [Silly Type Synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
        type C u a = u  -- Note 'a' unused

        foo :: (forall a. C u a -> C u a) -> u
        foo x = ...

        bar :: Num u => u
        bar = foo (\t -> t + t)

* From the (\t -> t+t) we get type  {Num d} =>  d -> d
  where d is fresh.

* Now unify with type of foo's arg, and we get:
        {Num (C d a)} =>  C d a -> C d a
  where a is fresh.

* Now abstract over the 'a', but float out the Num (C d a) constraint
  because it does not 'really' mention a.  (see exactTyVarsOfType)
  The arg to foo becomes
        \/\a -> \t -> t+t

* So we get a dict binding for Num (C d a), which is zonked to give
        a = ()
  [Note Sept 04: now that we are zonking quantified type variables
  on construction, the 'a' will be frozen as a regular tyvar on
  quantification, so the floated dict will still have type (C d a).
  Which renders this whole note moot; happily!]

* Then the \/\a abstraction has a zonked 'a' in it.

All very silly.   I think its harmless to ignore the problem.  We'll end up with
a \/\a in the final result but all the occurrences of a will be zonked to ()

************************************************************************
*                                                                      *
              Zonking types
*                                                                      *
************************************************************************

-}

-- | @tcGetGlobalTyCoVars@ returns a fully-zonked set of *scoped* tyvars free in
-- the environment. To improve subsequent calls to the same function it writes
-- the zonked set back into the environment. Note that this returns all
-- variables free in anything (term-level or type-level) in scope. We thus
-- don't have to worry about clashes with things that are not in scope, because
-- if they are reachable, then they'll be returned here.
tcGetGlobalTyCoVars :: TcM TcTyVarSet
tcGetGlobalTyCoVars
  = do { (TcLclEnv {tcl_tyvars = gtv_var}) <- getLclEnv
       ; gbl_tvs  <- readMutVar gtv_var
       ; gbl_tvs' <- zonkTyCoVarsAndFV gbl_tvs
       ; writeMutVar gtv_var gbl_tvs'
       ; return gbl_tvs' }

-- | Zonk a type without using the smart constructors; the result type
-- is available for inspection within the type-checking knot.
zonkTcTypeInKnot :: TcType -> TcM TcType
zonkTcTypeInKnot = mapType (zonkTcTypeMapper { tcm_smart = False }) ()

zonkTcTypeAndFV :: TcType -> TcM DTyCoVarSet
-- Zonk a type and take its free variables
-- With kind polymorphism it can be essential to zonk *first*
-- so that we find the right set of free variables.  Eg
--    forall k1. forall (a:k2). a
-- where k2:=k1 is in the substitution.  We don't want
-- k2 to look free in this type!
-- NB: This might be called from within the knot, so don't use
-- smart constructors. See Note [Zonking within the knot] in TcHsType
zonkTcTypeAndFV ty
  = tyCoVarsOfTypeDSet <$> zonkTcTypeInKnot ty

-- | Zonk a type and call 'candidateQTyVarsOfType' on it.
-- Works within the knot.
zonkTcTypeAndSplitDepVars :: TcType -> TcM CandidatesQTvs
zonkTcTypeAndSplitDepVars ty
  = candidateQTyVarsOfType <$> zonkTcTypeInKnot ty

zonkTcTypesAndSplitDepVars :: [TcType] -> TcM CandidatesQTvs
zonkTcTypesAndSplitDepVars tys
  = candidateQTyVarsOfTypes <$> mapM zonkTcTypeInKnot tys

zonkTyCoVar :: TyCoVar -> TcM TcType
-- Works on TyVars and TcTyVars
zonkTyCoVar tv | isTcTyVar tv = zonkTcTyVar tv
               | isTyVar   tv = mkTyVarTy <$> zonkTyCoVarKind tv
               | otherwise    = ASSERT2( isCoVar tv, ppr tv )
                                mkCoercionTy . mkCoVarCo <$> zonkTyCoVarKind tv
   -- Hackily, when typechecking type and class decls
   -- we have TyVars in scopeadded (only) in
   -- TcHsType.tcTyClTyVars, but it seems
   -- painful to make them into TcTyVars there

zonkTyCoVarsAndFV :: TyCoVarSet -> TcM TyCoVarSet
zonkTyCoVarsAndFV tycovars =
  tyCoVarsOfTypes <$> mapM zonkTyCoVar (nonDetEltsUniqSet tycovars)
  -- It's OK to use nonDetEltsUniqSet here because we immediately forget about
  -- the ordering by turning it into a nondeterministic set and the order
  -- of zonking doesn't matter for determinism.

-- Takes a list of TyCoVars, zonks them and returns a
-- deterministically ordered list of their free variables.
zonkTyCoVarsAndFVList :: [TyCoVar] -> TcM [TyCoVar]
zonkTyCoVarsAndFVList tycovars =
  tyCoVarsOfTypesList <$> mapM zonkTyCoVar tycovars

zonkTcTyVars :: [TcTyVar] -> TcM [TcType]
zonkTcTyVars tyvars = mapM zonkTcTyVar tyvars

-----------------  Types
zonkTyCoVarKind :: TyCoVar -> TcM TyCoVar
zonkTyCoVarKind tv = do { kind' <- zonkTcType (tyVarKind tv)
                        ; return (setTyVarKind tv kind') }

zonkTcTypes :: [TcType] -> TcM [TcType]
zonkTcTypes tys = mapM zonkTcType tys

{-
************************************************************************
*                                                                      *
              Zonking constraints
*                                                                      *
************************************************************************
-}

zonkImplication :: Implication -> TcM Implication
zonkImplication implic@(Implic { ic_skols  = skols
                               , ic_given  = given
                               , ic_wanted = wanted
                               , ic_info   = info })
  = do { skols'  <- mapM zonkTcTyCoVarBndr skols  -- Need to zonk their kinds!
                                                  -- as Trac #7230 showed
       ; given'  <- mapM zonkEvVar given
       ; info'   <- zonkSkolemInfo info
       ; wanted' <- zonkWCRec wanted
       ; return (implic { ic_skols  = skols'
                        , ic_given  = given'
                        , ic_wanted = wanted'
                        , ic_info   = info' }) }

zonkEvVar :: EvVar -> TcM EvVar
zonkEvVar var = do { ty' <- zonkTcType (varType var)
                   ; return (setVarType var ty') }


zonkWC :: WantedConstraints -> TcM WantedConstraints
zonkWC wc = zonkWCRec wc

zonkWCRec :: WantedConstraints -> TcM WantedConstraints
zonkWCRec (WC { wc_simple = simple, wc_impl = implic, wc_insol = insol })
  = do { simple' <- zonkSimples simple
       ; implic' <- mapBagM zonkImplication implic
       ; insol'  <- zonkSimples insol
       ; return (WC { wc_simple = simple', wc_impl = implic', wc_insol = insol' }) }

zonkSimples :: Cts -> TcM Cts
zonkSimples cts = do { cts' <- mapBagM zonkCt' cts
                     ; traceTc "zonkSimples done:" (ppr cts')
                     ; return cts' }

zonkCt' :: Ct -> TcM Ct
zonkCt' ct = zonkCt ct

{- Note [zonkCt behaviour]
zonkCt tries to maintain the canonical form of a Ct.  For example,
  - a CDictCan should stay a CDictCan;
  - a CTyEqCan should stay a CTyEqCan (if the LHS stays as a variable.).
  - a CHoleCan should stay a CHoleCan

Why?, for example:
- For CDictCan, the @TcSimplify.expandSuperClasses@ step, which runs after the
  simple wanted and plugin loop, looks for @CDictCan@s. If a plugin is in use,
  constraints are zonked before being passed to the plugin. This means if we
  don't preserve a canonical form, @expandSuperClasses@ fails to expand
  superclasses. This is what happened in Trac #11525.

- For CHoleCan, once we forget that it's a hole, we can never recover that info.

NB: we do not expect to see any CFunEqCans, because zonkCt is only
called on unflattened constraints.
NB: Constraints are always re-flattened etc by the canonicaliser in
@TcCanonical@ even if they come in as CDictCan. Only canonical constraints that
are actually in the inert set carry all the guarantees. So it is okay if zonkCt
creates e.g. a CDictCan where the cc_tyars are /not/ function free.
-}
zonkCt :: Ct -> TcM Ct
zonkCt ct@(CHoleCan { cc_ev = ev })
  = do { ev' <- zonkCtEvidence ev
       ; return $ ct { cc_ev = ev' } }
zonkCt ct@(CDictCan { cc_ev = ev, cc_tyargs = args })
  = do { ev'   <- zonkCtEvidence ev
       ; args' <- mapM zonkTcType args
       ; return $ ct { cc_ev = ev', cc_tyargs = args' } }
zonkCt ct@(CTyEqCan { cc_ev = ev, cc_tyvar = tv, cc_rhs = rhs })
  = do { ev'    <- zonkCtEvidence ev
       ; tv_ty' <- zonkTcTyVar tv
       ; case getTyVar_maybe tv_ty' of
           Just tv' -> do { rhs' <- zonkTcType rhs
                          ; return ct { cc_ev    = ev'
                                      , cc_tyvar = tv'
                                      , cc_rhs   = rhs' } }
           Nothing  -> return (mkNonCanonical ev') }
zonkCt ct
  = ASSERT( not (isCFunEqCan ct) )
  -- We do not expect to see any CFunEqCans, because zonkCt is only called on
  -- unflattened constraints.
    do { fl' <- zonkCtEvidence (cc_ev ct)
       ; return (mkNonCanonical fl') }

zonkCtEvidence :: CtEvidence -> TcM CtEvidence
zonkCtEvidence ctev@(CtGiven { ctev_pred = pred })
  = do { pred' <- zonkTcType pred
       ; return (ctev { ctev_pred = pred'}) }
zonkCtEvidence ctev@(CtWanted { ctev_pred = pred, ctev_dest = dest })
  = do { pred' <- zonkTcType pred
       ; let dest' = case dest of
                       EvVarDest ev -> EvVarDest $ setVarType ev pred'
                         -- necessary in simplifyInfer
                       HoleDest h   -> HoleDest h
       ; return (ctev { ctev_pred = pred', ctev_dest = dest' }) }
zonkCtEvidence ctev@(CtDerived { ctev_pred = pred })
  = do { pred' <- zonkTcType pred
       ; return (ctev { ctev_pred = pred' }) }

zonkSkolemInfo :: SkolemInfo -> TcM SkolemInfo
zonkSkolemInfo (SigSkol cx ty tv_prs)  = do { ty' <- zonkTcType ty
                                            ; return (SigSkol cx ty' tv_prs) }
zonkSkolemInfo (InferSkol ntys) = do { ntys' <- mapM do_one ntys
                                     ; return (InferSkol ntys') }
  where
    do_one (n, ty) = do { ty' <- zonkTcType ty; return (n, ty') }
zonkSkolemInfo skol_info = return skol_info

{-
%************************************************************************
%*                                                                      *
\subsection{Zonking -- the main work-horses: zonkTcType, zonkTcTyVar}
*                                                                      *
*              For internal use only!                                  *
*                                                                      *
************************************************************************

-}

-- zonkId is used *during* typechecking just to zonk the Id's type
zonkId :: TcId -> TcM TcId
zonkId id
  = do { ty' <- zonkTcType (idType id)
       ; return (Id.setIdType id ty') }

-- | A suitable TyCoMapper for zonking a type inside the knot, and
-- before all metavars are filled in.
zonkTcTypeMapper :: TyCoMapper () TcM
zonkTcTypeMapper = TyCoMapper
  { tcm_smart = True
  , tcm_tyvar = const zonkTcTyVar
  , tcm_covar = const (\cv -> mkCoVarCo <$> zonkTyCoVarKind cv)
  , tcm_hole  = hole
  , tcm_tybinder = \_env tv _vis -> ((), ) <$> zonkTcTyCoVarBndr tv }
  where
    hole :: () -> CoercionHole -> Role -> Type -> Type
         -> TcM Coercion
    hole _ h r t1 t2
      = do { contents <- unpackCoercionHole_maybe h
           ; case contents of
               Just co -> do { co <- zonkCo co
                             ; checkCoercionHole co h r t1 t2 }
               Nothing -> do { t1 <- zonkTcType t1
                             ; t2 <- zonkTcType t2
                             ; return $ mkHoleCo h r t1 t2 } }


-- For unbound, mutable tyvars, zonkType uses the function given to it
-- For tyvars bound at a for-all, zonkType zonks them to an immutable
--      type variable and zonks the kind too
zonkTcType :: TcType -> TcM TcType
zonkTcType = mapType zonkTcTypeMapper ()

-- | "Zonk" a coercion -- really, just zonk any types in the coercion
zonkCo :: Coercion -> TcM Coercion
zonkCo = mapCoercion zonkTcTypeMapper ()

zonkTcTyCoVarBndr :: TcTyCoVar -> TcM TcTyCoVar
-- A tyvar binder is never a unification variable (MetaTv),
-- rather it is always a skolems.  BUT it may have a kind
-- that has not yet been zonked, and may include kind
-- unification variables.
zonkTcTyCoVarBndr tyvar
    -- can't use isCoVar, because it looks at a TyCon. Argh.
  = ASSERT2( isImmutableTyVar tyvar || (not $ isTyVar tyvar), pprTyVar tyvar )
    updateTyVarKindM zonkTcType tyvar

zonkTcTyVarBinder :: TyVarBndr TcTyVar vis -> TcM (TyVarBndr TcTyVar vis)
zonkTcTyVarBinder (TvBndr tv vis)
  = do { tv' <- zonkTcTyCoVarBndr tv
       ; return (TvBndr tv' vis) }

zonkTcTyVar :: TcTyVar -> TcM TcType
-- Simply look through all Flexis
zonkTcTyVar tv
  | isTcTyVar tv
  = case tcTyVarDetails tv of
      SkolemTv {}   -> zonk_kind_and_return
      RuntimeUnk {} -> zonk_kind_and_return
      MetaTv { mtv_ref = ref }
         -> do { cts <- readMutVar ref
               ; case cts of
                    Flexi       -> zonk_kind_and_return
                    Indirect ty -> zonkTcType ty }

  | otherwise -- coercion variable
  = zonk_kind_and_return
  where
    zonk_kind_and_return = do { z_tv <- zonkTyCoVarKind tv
                              ; return (mkTyVarTy z_tv) }

-- Variant that assumes that any result of zonking is still a TyVar.
-- Should be used only on skolems and SigTvs
zonkTcTyVarToTyVar :: TcTyVar -> TcM TcTyVar
zonkTcTyVarToTyVar tv
  = do { ty <- zonkTcTyVar tv
       ; return (tcGetTyVar "zonkTcTyVarToVar" ty) }

{-
%************************************************************************
%*                                                                      *
                 Tidying
*                                                                      *
************************************************************************
-}

zonkTidyTcType :: TidyEnv -> TcType -> TcM (TidyEnv, TcType)
zonkTidyTcType env ty = do { ty' <- zonkTcType ty
                           ; return (tidyOpenType env ty') }

zonkTidyOrigin :: TidyEnv -> CtOrigin -> TcM (TidyEnv, CtOrigin)
zonkTidyOrigin env (GivenOrigin skol_info)
  = do { skol_info1 <- zonkSkolemInfo skol_info
       ; let skol_info2 = tidySkolemInfo env skol_info1
       ; return (env, GivenOrigin skol_info2) }
zonkTidyOrigin env orig@(TypeEqOrigin { uo_actual   = act
                                      , uo_expected = exp })
  = do { (env1, act') <- zonkTidyTcType env  act
       ; (env2, exp') <- zonkTidyTcType env1 exp
       ; return ( env2, orig { uo_actual   = act'
                             , uo_expected = exp' }) }
zonkTidyOrigin env (KindEqOrigin ty1 m_ty2 orig t_or_k)
  = do { (env1, ty1')   <- zonkTidyTcType env  ty1
       ; (env2, m_ty2') <- case m_ty2 of
                             Just ty2 -> second Just <$> zonkTidyTcType env1 ty2
                             Nothing  -> return (env1, Nothing)
       ; (env3, orig')  <- zonkTidyOrigin env2 orig
       ; return (env3, KindEqOrigin ty1' m_ty2' orig' t_or_k) }
zonkTidyOrigin env (FunDepOrigin1 p1 l1 p2 l2)
  = do { (env1, p1') <- zonkTidyTcType env  p1
       ; (env2, p2') <- zonkTidyTcType env1 p2
       ; return (env2, FunDepOrigin1 p1' l1 p2' l2) }
zonkTidyOrigin env (FunDepOrigin2 p1 o1 p2 l2)
  = do { (env1, p1') <- zonkTidyTcType env  p1
       ; (env2, p2') <- zonkTidyTcType env1 p2
       ; (env3, o1') <- zonkTidyOrigin env2 o1
       ; return (env3, FunDepOrigin2 p1' o1' p2' l2) }
zonkTidyOrigin env orig = return (env, orig)

----------------
tidyCt :: TidyEnv -> Ct -> Ct
-- Used only in error reporting
-- Also converts it to non-canonical
tidyCt env ct
  = case ct of
     CHoleCan { cc_ev = ev }
       -> ct { cc_ev = tidy_ev env ev }
     _ -> mkNonCanonical (tidy_ev env (ctEvidence ct))
  where
    tidy_ev :: TidyEnv -> CtEvidence -> CtEvidence
     -- NB: we do not tidy the ctev_evar field because we don't
     --     show it in error messages
    tidy_ev env ctev@(CtGiven { ctev_pred = pred })
      = ctev { ctev_pred = tidyType env pred }
    tidy_ev env ctev@(CtWanted { ctev_pred = pred })
      = ctev { ctev_pred = tidyType env pred }
    tidy_ev env ctev@(CtDerived { ctev_pred = pred })
      = ctev { ctev_pred = tidyType env pred }

----------------
tidyEvVar :: TidyEnv -> EvVar -> EvVar
tidyEvVar env var = setVarType var (tidyType env (varType var))

----------------
tidySkolemInfo :: TidyEnv -> SkolemInfo -> SkolemInfo
tidySkolemInfo env (DerivSkol ty)         = DerivSkol (tidyType env ty)
tidySkolemInfo env (SigSkol cx ty tv_prs) = tidySigSkol env cx ty tv_prs
tidySkolemInfo env (InferSkol ids)        = InferSkol (mapSnd (tidyType env) ids)
tidySkolemInfo env (UnifyForAllSkol ty)   = UnifyForAllSkol (tidyType env ty)
tidySkolemInfo _   info                   = info

tidySigSkol :: TidyEnv -> UserTypeCtxt
            -> TcType -> [(Name,TcTyVar)] -> SkolemInfo
-- We need to take special care when tidying SigSkol
-- See Note [SigSkol SkolemInfo] in TcRnTypes
tidySigSkol env cx ty tv_prs
  = SigSkol cx (tidy_ty env ty) tv_prs'
  where
    tv_prs' = mapSnd (tidyTyVarOcc env) tv_prs
    inst_env = mkNameEnv tv_prs'

    tidy_ty env (ForAllTy (TvBndr tv vis) ty)
      = ForAllTy (TvBndr tv' vis) (tidy_ty env' ty)
      where
        (env', tv') = tidy_tv_bndr env tv

    tidy_ty env (FunTy arg res)
      = FunTy (tidyType env arg) (tidy_ty env res)

    tidy_ty env ty = tidyType env ty

    tidy_tv_bndr :: TidyEnv -> TyVar -> (TidyEnv, TyVar)
    tidy_tv_bndr env@(occ_env, subst) tv
      | Just tv' <- lookupNameEnv inst_env (tyVarName tv)
      = ((occ_env, extendVarEnv subst tv tv'), tv')

      | otherwise
      = tidyTyCoVarBndr env tv

-------------------------------------------------------------------------
{-
%************************************************************************
%*                                                                      *
             Levity polymorphism checks
*                                                                      *
************************************************************************

See Note [Levity polymorphism checking] in DsMonad

-}

-- | According to the rules around representation polymorphism
-- (see https://ghc.haskell.org/trac/ghc/wiki/NoSubKinds), no binder
-- can have a representation-polymorphic type. This check ensures
-- that we respect this rule. It is a bit regrettable that this error
-- occurs in zonking, after which we should have reported all errors.
-- But it's hard to see where else to do it, because this can be discovered
-- only after all solving is done. And, perhaps most importantly, this
-- isn't really a compositional property of a type system, so it's
-- not a terrible surprise that the check has to go in an awkward spot.
ensureNotLevPoly :: Type  -- its zonked type
                 -> SDoc  -- where this happened
                 -> TcM ()
ensureNotLevPoly ty doc
  = whenNoErrs $   -- sometimes we end up zonking bogus definitions of type
                   -- forall a. a. See, for example, test ghci/scripts/T9140
    checkForLevPoly doc ty

  -- See Note [Levity polymorphism checking] in DsMonad
checkForLevPoly :: SDoc -> Type -> TcM ()
checkForLevPoly = checkForLevPolyX addErr

checkForLevPolyX :: Monad m
                 => (SDoc -> m ())  -- how to report an error
                 -> SDoc -> Type -> m ()
checkForLevPolyX add_err extra ty
  | isTypeLevPoly ty
  = add_err (formatLevPolyErr ty $$ extra)
  | otherwise
  = return ()

formatLevPolyErr :: Type  -- levity-polymorphic type
                 -> SDoc
formatLevPolyErr ty
  = hang (text "A levity-polymorphic type is not allowed here:")
       2 (vcat [ text "Type:" <+> ppr tidy_ty
               , text "Kind:" <+> ppr tidy_ki ])
  where
    (tidy_env, tidy_ty) = tidyOpenType emptyTidyEnv ty
    tidy_ki             = tidyType tidy_env (typeKind ty)
