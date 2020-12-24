{-# LANGUAGE CPP             #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TupleSections   #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Monadic type operations
--
-- This module contains monadic operations over types that contain mutable type
-- variables.
module GHC.Tc.Utils.TcMType (
  TcTyVar, TcKind, TcType, TcTauType, TcThetaType, TcTyVarSet,

  --------------------------------
  -- Creating new mutable type variables
  newFlexiTyVar,
  newNamedFlexiTyVar,
  newFlexiTyVarTy,              -- Kind -> TcM TcType
  newFlexiTyVarTys,             -- Int -> Kind -> TcM [TcType]
  newOpenFlexiTyVar, newOpenFlexiTyVarTy, newOpenTypeKind,
  newMetaKindVar, newMetaKindVars, newMetaTyVarTyAtLevel,
  newAnonMetaTyVar, cloneMetaTyVar,
  newCycleBreakerTyVar,

  newMultiplicityVar,
  readMetaTyVar, writeMetaTyVar, writeMetaTyVarRef,
  newTauTvDetailsAtLevel, newMetaDetails, newMetaTyVarName,
  isFilledMetaTyVar_maybe, isFilledMetaTyVar, isUnfilledMetaTyVar,

  --------------------------------
  -- Creating new evidence variables
  newEvVar, newEvVars, newDict,
  newWanted, newWanteds, cloneWanted, cloneWC,
  emitWanted, emitWantedEq, emitWantedEvVar, emitWantedEvVars,
  emitDerivedEqs,
  newTcEvBinds, newNoTcEvBinds, addTcEvBind,
  emitNewExprHole,

  newCoercionHole, fillCoercionHole, isFilledCoercionHole,
  unpackCoercionHole, unpackCoercionHole_maybe,
  checkCoercionHole,

  newImplication,

  --------------------------------
  -- Instantiation
  newMetaTyVars, newMetaTyVarX, newMetaTyVarsX,
  newMetaTyVarTyVarX,
  newTyVarTyVar, cloneTyVarTyVar,
  newPatSigTyVar, newSkolemTyVar, newWildCardX,

  --------------------------------
  -- Expected types
  ExpType(..), ExpSigmaType, ExpRhoType,
  mkCheckExpType, newInferExpType, tcInfer,
  readExpType, readExpType_maybe, readScaledExpType,
  expTypeToType, scaledExpTypeToType,
  checkingExpType_maybe, checkingExpType,
  inferResultToType, fillInferResult, promoteTcType,

  --------------------------------
  -- Zonking and tidying
  zonkTidyTcType, zonkTidyTcTypes, zonkTidyOrigin,
  tidyEvVar, tidyCt, tidyHole, tidySkolemInfo,
    zonkTcTyVar, zonkTcTyVars,
  zonkTcTyVarToTyVar, zonkInvisTVBinder,
  zonkTyCoVarsAndFV, zonkTcTypeAndFV, zonkDTyCoVarSetAndFV,
  zonkTyCoVarsAndFVList,

  zonkTcType, zonkTcTypes, zonkCo,
  zonkTyCoVarKind, zonkTyCoVarKindBinder,
  zonkEvVar, zonkWC, zonkImplication, zonkSimples,
  zonkId, zonkCoVar,
  zonkCt, zonkSkolemInfo,

  ---------------------------------
  -- Promotion, defaulting, skolemisation
  defaultTyVar, promoteMetaTyVarTo, promoteTyVarSet,
  quantifyTyVars, isQuantifiableTv,
  skolemiseUnboundMetaTyVar, zonkAndSkolemise, skolemiseQuantifiedTyVar,

  candidateQTyVarsOfType,  candidateQTyVarsOfKind,
  candidateQTyVarsOfTypes, candidateQTyVarsOfKinds,
  CandidatesQTvs(..), delCandidates,
  candidateKindVars, partitionCandidates,

  ------------------------------
  -- Levity polymorphism
  ensureNotLevPoly, checkForLevPoly, checkForLevPolyX, formatLevPolyErr
  ) where

#include "HsVersions.h"

-- friends:
import GHC.Prelude

import {-# SOURCE #-}   GHC.Tc.Utils.Unify( unifyType {- , unifyKind -} )

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr
import GHC.Tc.Utils.TcType
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Core.Coercion
import GHC.Core.Class
import GHC.Types.Var
import GHC.Core.Predicate
import GHC.Tc.Types.Origin

-- others:
import GHC.Tc.Utils.Monad        -- TcType, amongst others
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.Types.Id as Id
import GHC.Types.Name
import GHC.Types.Var.Set
import GHC.Builtin.Types
import GHC.Types.Var.Env
import GHC.Types.Name.Env
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.FastString
import GHC.Data.Bag
import GHC.Data.Pair
import GHC.Types.Unique.Set
import GHC.Driver.Session
import GHC.Driver.Ppr
import qualified GHC.LanguageExtensions as LangExt
import GHC.Types.Basic ( TypeOrKind(..) )

import Control.Monad
import GHC.Data.Maybe
import Control.Arrow    ( second )
import qualified Data.Semigroup as Semi

{-
************************************************************************
*                                                                      *
        Kind variables
*                                                                      *
************************************************************************
-}

newMetaKindVar :: TcM TcKind
newMetaKindVar
  = do { details <- newMetaDetails TauTv
       ; name    <- newMetaTyVarName (fsLit "k")
                    -- All MetaKindVars are called "k"
                    -- They may be jiggled by tidying
       ; let kv = mkTcTyVar name liftedTypeKind details
       ; traceTc "newMetaKindVar" (ppr kv)
       ; return (mkTyVarTy kv) }

newMetaKindVars :: Int -> TcM [TcKind]
newMetaKindVars n = replicateM n newMetaKindVar

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
                 ; return (mkLocalIdOrCoVar name Many ty) }

newWanted :: CtOrigin -> Maybe TypeOrKind -> PredType -> TcM CtEvidence
-- Deals with both equality and non-equality predicates
newWanted orig t_or_k pty
  = do loc <- getCtLocM orig t_or_k
       d <- if isEqPrimPred pty then HoleDest  <$> newCoercionHole pty
                                else EvVarDest <$> newEvVar pty
       return $ CtWanted { ctev_dest = d
                         , ctev_pred = pty
                         , ctev_nosh = WDeriv
                         , ctev_loc = loc }

newWanteds :: CtOrigin -> ThetaType -> TcM [CtEvidence]
newWanteds orig = mapM (newWanted orig Nothing)

----------------------------------------------
-- Cloning constraints
----------------------------------------------

cloneWanted :: Ct -> TcM Ct
cloneWanted ct
  | ev@(CtWanted { ctev_pred = pty }) <- ctEvidence ct
  = do { co_hole <- newCoercionHole pty
       ; return (mkNonCanonical (ev { ctev_dest = HoleDest co_hole })) }
  | otherwise
  = return ct

cloneWC :: WantedConstraints -> TcM WantedConstraints
-- Clone all the evidence bindings in
--   a) the ic_bind field of any implications
--   b) the CoercionHoles of any wanted constraints
-- so that solving the WantedConstraints will not have any visible side
-- effect, /except/ from causing unifications
cloneWC wc@(WC { wc_simple = simples, wc_impl = implics })
  = do { simples' <- mapBagM cloneWanted simples
       ; implics' <- mapBagM cloneImplication implics
       ; return (wc { wc_simple = simples', wc_impl = implics' }) }

cloneImplication :: Implication -> TcM Implication
cloneImplication implic@(Implic { ic_binds = binds, ic_wanted = inner_wanted })
  = do { binds'        <- cloneEvBindsVar binds
       ; inner_wanted' <- cloneWC inner_wanted
       ; return (implic { ic_binds = binds', ic_wanted = inner_wanted' }) }

----------------------------------------------
-- Emitting constraints
----------------------------------------------

-- | Emits a new Wanted. Deals with both equalities and non-equalities.
emitWanted :: CtOrigin -> TcPredType -> TcM EvTerm
emitWanted origin pty
  = do { ev <- newWanted origin Nothing pty
       ; emitSimple $ mkNonCanonical ev
       ; return $ ctEvTerm ev }

emitDerivedEqs :: CtOrigin -> [(TcType,TcType)] -> TcM ()
-- Emit some new derived nominal equalities
emitDerivedEqs origin pairs
  | null pairs
  = return ()
  | otherwise
  = do { loc <- getCtLocM origin Nothing
       ; emitSimples (listToBag (map (mk_one loc) pairs)) }
  where
    mk_one loc (ty1, ty2)
       = mkNonCanonical $
         CtDerived { ctev_pred = mkPrimEqPred ty1 ty2
                   , ctev_loc = loc }

-- | Emits a new equality constraint
emitWantedEq :: CtOrigin -> TypeOrKind -> Role -> TcType -> TcType -> TcM Coercion
emitWantedEq origin t_or_k role ty1 ty2
  = do { hole <- newCoercionHole pty
       ; loc <- getCtLocM origin (Just t_or_k)
       ; emitSimple $ mkNonCanonical $
         CtWanted { ctev_pred = pty, ctev_dest = HoleDest hole
                  , ctev_nosh = WDeriv, ctev_loc = loc }
       ; return (HoleCo hole) }
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

-- | Emit a new wanted expression hole
emitNewExprHole :: OccName         -- of the hole
                -> Type -> TcM HoleExprRef
emitNewExprHole occ ty
  = do { u <- newUnique
       ; ref <- newTcRef (pprPanic "unfilled unbound-variable evidence" (ppr u))
       ; let her = HER ref ty u

       ; loc <- getCtLocM (ExprHoleOrigin occ) (Just TypeLevel)

       ; let hole = Hole { hole_sort = ExprHole her
                         , hole_occ  = occ
                         , hole_ty   = ty
                         , hole_loc  = loc }
       ; emitHole hole
       ; return her }

newDict :: Class -> [TcType] -> TcM DictId
newDict cls tys
  = do { name <- newSysName (mkDictOcc (getOccName cls))
       ; return (mkLocalId name Many (mkClassPred cls tys)) }

predTypeOccName :: PredType -> OccName
predTypeOccName ty = case classifyPredType ty of
    ClassPred cls _ -> mkDictOcc (getOccName cls)
    EqPred {}       -> mkVarOccFS (fsLit "co")
    IrredPred {}    -> mkVarOccFS (fsLit "irred")
    ForAllPred {}   -> mkVarOccFS (fsLit "df")

-- | Create a new 'Implication' with as many sensible defaults for its fields
-- as possible. Note that the 'ic_tclvl', 'ic_binds', and 'ic_info' fields do
-- /not/ have sensible defaults, so they are initialized with lazy thunks that
-- will 'panic' if forced, so one should take care to initialize these fields
-- after creation.
--
-- This is monadic to look up the 'TcLclEnv', which is used to initialize
-- 'ic_env', and to set the -Winaccessible-code flag. See
-- Note [Avoid -Winaccessible-code when deriving] in "GHC.Tc.TyCl.Instance".
newImplication :: TcM Implication
newImplication
  = do env <- getLclEnv
       warn_inaccessible <- woptM Opt_WarnInaccessibleCode
       return (implicationPrototype { ic_env = env
                                    , ic_warn_inaccessible = warn_inaccessible })

{-
************************************************************************
*                                                                      *
        Coercion holes
*                                                                      *
************************************************************************
-}

newCoercionHole :: TcPredType -> TcM CoercionHole
newCoercionHole pred_ty
  = do { co_var <- newEvVar pred_ty
       ; traceTc "New coercion hole:" (ppr co_var)
       ; ref <- newMutVar Nothing
       ; return $ CoercionHole { ch_co_var = co_var, ch_ref = ref } }

-- | Put a value in a coercion hole
fillCoercionHole :: CoercionHole -> Coercion -> TcM ()
fillCoercionHole (CoercionHole { ch_ref = ref, ch_co_var = cv }) co
  = do {
#if defined(DEBUG)
       ; cts <- readTcRef ref
       ; whenIsJust cts $ \old_co ->
         pprPanic "Filling a filled coercion hole" (ppr cv $$ ppr co $$ ppr old_co)
#endif
       ; traceTc "Filling coercion hole" (ppr cv <+> text ":=" <+> ppr co)
       ; writeTcRef ref (Just co) }

-- | Is a coercion hole filled in?
isFilledCoercionHole :: CoercionHole -> TcM Bool
isFilledCoercionHole (CoercionHole { ch_ref = ref }) = isJust <$> readTcRef ref

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
unpackCoercionHole_maybe (CoercionHole { ch_ref = ref }) = readTcRef ref

-- | Check that a coercion is appropriate for filling a hole. (The hole
-- itself is needed only for printing.
-- Always returns the checked coercion, but this return value is necessary
-- so that the input coercion is forced only when the output is forced.
checkCoercionHole :: CoVar -> Coercion -> TcM Coercion
checkCoercionHole cv co
  | debugIsOn
  = do { cv_ty <- zonkTcType (varType cv)
                  -- co is already zonked, but cv might not be
       ; return $
         ASSERT2( ok cv_ty
                , (text "Bad coercion hole" <+>
                   ppr cv <> colon <+> vcat [ ppr t1, ppr t2, ppr role
                                            , ppr cv_ty ]) )
         co }
  | otherwise
  = return co

  where
    (Pair t1 t2, role) = coercionKindRole co
    ok cv_ty | EqPred cv_rel cv_t1 cv_t2 <- classifyPredType cv_ty
             =  t1 `eqType` cv_t1
             && t2 `eqType` cv_t2
             && role == eqRelRole cv_rel
             | otherwise
             = False

{- **********************************************************************
*
                      ExpType functions
*
********************************************************************** -}

{- Note [ExpType]
~~~~~~~~~~~~~~~~~
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

See also [wiki:typechecking]

Note [TcLevel of ExpType]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  data G a where
    MkG :: G Bool

  foo MkG = True

This is a classic untouchable-variable / ambiguous GADT return type
scenario. But, with ExpTypes, we'll be inferring the type of the RHS.
We thus must track a TcLevel in an Inferring ExpType. If we try to
fill the ExpType and find that the TcLevels don't work out, we fill
the ExpType with a tau-tv at the low TcLevel, hopefully to be worked
out later by some means -- see fillInferResult, and Note [fillInferResult]

This behaviour triggered in test gadt/gadt-escape1.
-}

-- actual data definition is in GHC.Tc.Utils.TcType

newInferExpType :: TcM ExpType
newInferExpType
  = do { u <- newUnique
       ; tclvl <- getTcLevel
       ; traceTc "newInferExpType" (ppr u <+> ppr tclvl)
       ; ref <- newMutVar Nothing
       ; return (Infer (IR { ir_uniq = u, ir_lvl = tclvl
                           , ir_ref = ref })) }

-- | Extract a type out of an ExpType, if one exists. But one should always
-- exist. Unless you're quite sure you know what you're doing.
readExpType_maybe :: ExpType -> TcM (Maybe TcType)
readExpType_maybe (Check ty)                   = return (Just ty)
readExpType_maybe (Infer (IR { ir_ref = ref})) = readMutVar ref

-- | Same as readExpType, but for Scaled ExpTypes
readScaledExpType :: Scaled ExpType -> TcM (Scaled Type)
readScaledExpType (Scaled m exp_ty)
  = do { ty <- readExpType exp_ty
       ; return (Scaled m ty) }

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
checkingExpType_maybe (Infer {}) = Nothing

-- | Returns the expected type when in checking mode. Panics if in inference
-- mode.
checkingExpType :: String -> ExpType -> TcType
checkingExpType _   (Check ty) = ty
checkingExpType err et         = pprPanic "checkingExpType" (text err $$ ppr et)

scaledExpTypeToType :: Scaled ExpType -> TcM (Scaled TcType)
scaledExpTypeToType (Scaled m exp_ty)
  = do { ty <- expTypeToType exp_ty
       ; return (Scaled m ty) }

-- | Extracts the expected type if there is one, or generates a new
-- TauTv if there isn't.
expTypeToType :: ExpType -> TcM TcType
expTypeToType (Check ty)      = return ty
expTypeToType (Infer inf_res) = inferResultToType inf_res

inferResultToType :: InferResult -> TcM Type
inferResultToType (IR { ir_uniq = u, ir_lvl = tc_lvl
                      , ir_ref = ref })
  = do { mb_inferred_ty <- readTcRef ref
       ; tau <- case mb_inferred_ty of
            Just ty -> do { ensureMonoType ty
                            -- See Note [inferResultToType]
                          ; return ty }
            Nothing -> do { rr  <- newMetaTyVarTyAtLevel tc_lvl runtimeRepTy
                          ; tau <- newMetaTyVarTyAtLevel tc_lvl (tYPE rr)
                            -- See Note [TcLevel of ExpType]
                          ; writeMutVar ref (Just tau)
                          ; return tau }
       ; traceTc "Forcing ExpType to be monomorphic:"
                 (ppr u <+> text ":=" <+> ppr tau)
       ; return tau }

{- Note [inferResultToType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
expTypeToType and inferResultType convert an InferResult to a monotype.
It must be a monotype because if the InferResult isn't already filled in,
we fill it in with a unification variable (hence monotype).  So to preserve
order-independence we check for mono-type-ness even if it *is* filled in
already.

See also Note [TcLevel of ExpType] above, and
Note [fillInferResult].
-}

-- | Infer a type using a fresh ExpType
-- See also Note [ExpType] in "GHC.Tc.Utils.TcMType"
tcInfer :: (ExpSigmaType -> TcM a) -> TcM (a, TcSigmaType)
tcInfer tc_check
  = do { res_ty <- newInferExpType
       ; result <- tc_check res_ty
       ; res_ty <- readExpType res_ty
       ; return (result, res_ty) }

fillInferResult :: TcType -> InferResult -> TcM TcCoercionN
-- If co = fillInferResult t1 t2
--    => co :: t1 ~ t2
-- See Note [fillInferResult]
fillInferResult act_res_ty (IR { ir_uniq = u, ir_lvl = res_lvl
                               , ir_ref = ref })
  = do { mb_exp_res_ty <- readTcRef ref
       ; case mb_exp_res_ty of
            Just exp_res_ty
               -> do { traceTc "Joining inferred ExpType" $
                       ppr u <> colon <+> ppr act_res_ty <+> char '~' <+> ppr exp_res_ty
                     ; cur_lvl <- getTcLevel
                     ; unless (cur_lvl `sameDepthAs` res_lvl) $
                       ensureMonoType act_res_ty
                     ; unifyType Nothing act_res_ty exp_res_ty }
            Nothing
               -> do { traceTc "Filling inferred ExpType" $
                       ppr u <+> text ":=" <+> ppr act_res_ty
                     ; (prom_co, act_res_ty) <- promoteTcType res_lvl act_res_ty
                     ; writeTcRef ref (Just act_res_ty)
                     ; return prom_co }
     }


{- Note [fillInferResult]
~~~~~~~~~~~~~~~~~~~~~~~~~
When inferring, we use fillInferResult to "fill in" the hole in InferResult
   data InferResult = IR { ir_uniq :: Unique
                         , ir_lvl  :: TcLevel
                         , ir_ref  :: IORef (Maybe TcType) }

There are two things to worry about:

1. What if it is under a GADT or existential pattern match?
   - GADTs: a unification variable (and Infer's hole is similar) is untouchable
   - Existentials: be careful about skolem-escape

2. What if it is filled in more than once?  E.g. multiple branches of a case
     case e of
        T1 -> e1
        T2 -> e2

Our typing rules are:

* The RHS of a existential or GADT alternative must always be a
  monotype, regardless of the number of alternatives.

* Multiple non-existential/GADT branches can have (the same)
  higher rank type (#18412).  E.g. this is OK:
      case e of
        True  -> hr
        False -> hr
  where hr:: (forall a. a->a) -> Int
  c.f. Section 7.1 of "Practical type inference for arbitrary-rank types"
       We use choice (2) in that Section.
       (GHC 8.10 and earlier used choice (1).)

  But note that
      case e of
        True  -> hr
        False -> \x -> hr x
  will fail, because we still /infer/ both branches, so the \x will get
  a (monotype) unification variable, which will fail to unify with
  (forall a. a->a)

For (1) we can detect the GADT/existential situation by seeing that
the current TcLevel is greater than that stored in ir_lvl of the Infer
ExpType.  We bump the level whenever we go past a GADT/existential match.

Then, before filling the hole use promoteTcType to promote the type
to the outer ir_lvl.  promoteTcType does this
  - create a fresh unification variable alpha at level ir_lvl
  - emits an equality alpha[ir_lvl] ~ ty
  - fills the hole with alpha
That forces the type to be a monotype (since unification variables can
only unify with monotypes); and catches skolem-escapes because the
alpha is untouchable until the equality floats out.

For (2), we simply look to see if the hole is filled already.
  - if not, we promote (as above) and fill the hole
  - if it is filled, we simply unify with the type that is
    already there

There is one wrinkle.  Suppose we have
   case e of
      T1 -> e1 :: (forall a. a->a) -> Int
      G2 -> e2
where T1 is not GADT or existential, but G2 is a GADT.  Then supppose the
T1 alternative fills the hole with (forall a. a->a) -> Int, which is fine.
But now the G2 alternative must not *just* unify with that else we'd risk
allowing through (e2 :: (forall a. a->a) -> Int).  If we'd checked G2 first
we'd have filled the hole with a unification variable, which enforces a
monotype.

So if we check G2 second, we still want to emit a constraint that restricts
the RHS to be a monotype. This is done by ensureMonoType, and it works
by simply generating a constraint (alpha ~ ty), where alpha is a fresh
unification variable.  We discard the evidence.

-}

{- *********************************************************************
*                                                                      *
              Promoting types
*                                                                      *
********************************************************************* -}

ensureMonoType :: TcType -> TcM ()
-- Assuming that the argument type is of kind (TYPE r),
-- ensure that it is a /monotype/
-- If it is not a monotype we can see right away (since unification
-- varibles and type-function applications stand for monotypes), but
-- we emit a Wanted equality just to delay the error message until later
ensureMonoType res_ty
  | isTauTy res_ty   -- isTauTy doesn't need zonking or anything
  = return ()
  | otherwise
  = do { mono_ty <- newOpenFlexiTyVarTy
       ; let eq_orig = TypeEqOrigin { uo_actual   = res_ty
                                    , uo_expected = mono_ty
                                    , uo_thing    = Nothing
                                    , uo_visible  = False }

       ; _co <- emitWantedEq eq_orig TypeLevel Nominal res_ty mono_ty
       ; return () }

promoteTcType :: TcLevel -> TcType -> TcM (TcCoercionN, TcType)
-- See Note [Promoting a type]
-- See also Note [fillInferResult]
-- promoteTcType level ty = (co, ty')
--   * Returns ty'  whose max level is just 'level'
--             and  whose kind is ~# to the kind of 'ty'
--             and  whose kind has form TYPE rr
--   * and co :: ty ~ ty'
--   * and emits constraints to justify the coercion
--
-- NB: we expect that 'ty' has already kind (TYPE rr) for
--     some rr::RuntimeRep.  It is, after all, the type of a term.
promoteTcType dest_lvl ty
  = do { cur_lvl <- getTcLevel
       ; if (cur_lvl `sameDepthAs` dest_lvl)
         then return (mkTcNomReflCo ty, ty)
         else promote_it }
  where
    promote_it :: TcM (TcCoercion, TcType)
    promote_it  -- Emit a constraint  (alpha :: TYPE rr) ~ ty
                -- where alpha and rr are fresh and from level dest_lvl
      = do { rr      <- newMetaTyVarTyAtLevel dest_lvl runtimeRepTy
           ; prom_ty <- newMetaTyVarTyAtLevel dest_lvl (tYPE rr)
           ; let eq_orig = TypeEqOrigin { uo_actual   = ty
                                        , uo_expected = prom_ty
                                        , uo_thing    = Nothing
                                        , uo_visible  = False }

           ; co <- emitWantedEq eq_orig TypeLevel Nominal ty prom_ty
           ; return (co, prom_ty) }

{- Note [Promoting a type]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#12427)

  data T where
    MkT :: (Int -> Int) -> a -> T

  h y = case y of MkT v w -> v

We'll infer the RHS type with an expected type ExpType of
  (IR { ir_lvl = l, ir_ref = ref, ... )
where 'l' is the TcLevel of the RHS of 'h'.  Then the MkT pattern
match will increase the level, so we'll end up in tcSubType, trying to
unify the type of v,
  v :: Int -> Int
with the expected type.  But this attempt takes place at level (l+1),
rightly so, since v's type could have mentioned existential variables,
(like w's does) and we want to catch that.

So we
  - create a new meta-var alpha[l+1]
  - fill in the InferRes ref cell 'ref' with alpha
  - emit an equality constraint, thus
        [W] alpha[l+1] ~ (Int -> Int)

That constraint will float outwards, as it should, unless v's
type mentions a skolem-captured variable.

This approach fails if v has a higher rank type; see
Note [Promotion and higher rank types]


Note [Promotion and higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If v had a higher-rank type, say v :: (forall a. a->a) -> Int,
then we'd emit an equality
        [W] alpha[l+1] ~ ((forall a. a->a) -> Int)
which will sadly fail because we can't unify a unification variable
with a polytype.  But there is nothing really wrong with the program
here.

We could just about solve this by "promote the type" of v, to expose
its polymorphic "shape" while still leaving constraints that will
prevent existential escape.  But we must be careful!  Exposing
the "shape" of the type is precisely what we must NOT do under
a GADT pattern match!  So in this case we might promote the type
to
        (forall a. a->a) -> alpha[l+1]
and emit the constraint
        [W] alpha[l+1] ~ Int
Now the promoted type can fill the ref cell, while the emitted
equality can float or not, according to the usual rules.

But that's not quite right!  We are exposing the arrow! We could
deal with that too:
        (forall a. mu[l+1] a a) -> alpha[l+1]
with constraints
        [W] alpha[l+1] ~ Int
        [W] mu[l+1] ~ (->)
Here we abstract over the '->' inside the forall, in case that
is subject to an equality constraint from a GADT match.

Note that we kept the outer (->) because that's part of
the polymorphic "shape".  And because of impredicativity,
GADT matches can't give equalities that affect polymorphic
shape.

This reasoning just seems too complicated, so I decided not
to do it.  These higher-rank notes are just here to record
the thinking.
-}


{- *********************************************************************
*                                                                      *
        MetaTvs (meta type variables; mutable)
*                                                                      *
********************************************************************* -}

{- Note [TyVarTv]
~~~~~~~~~~~~~~~~~
A TyVarTv can unify with type *variables* only, including other TyVarTvs and
skolems. Sometimes, they can unify with type variables that the user would
rather keep distinct; see #11203 for an example.  So, any client of this
function needs to either allow the TyVarTvs to unify with each other or check
that they don't (say, with a call to findDubTyVarTvs).

Before #15050 this (under the name SigTv) was used for ScopedTypeVariables in
patterns, to make sure these type variables only refer to other type variables,
but this restriction was dropped, and ScopedTypeVariables can now refer to full
types (GHC Proposal 29).

The remaining uses of newTyVarTyVars are
* In kind signatures, see
  GHC.Tc.TyCl Note [Inferring kinds for type declarations]
          and Note [Kind checking for GADTs]
* In partial type signatures, see Note [Quantified variables in partial type signatures]
-}

newMetaTyVarName :: FastString -> TcM Name
-- Makes a /System/ Name, which is eagerly eliminated by
-- the unifier; see GHC.Tc.Utils.Unify.nicer_to_update_tv1, and
-- GHC.Tc.Solver.Canonical.canEqTyVarTyVar (nicer_to_update_tv2)
newMetaTyVarName str
  = do { uniq <- newUnique
       ; return (mkSystemName uniq (mkTyVarOccFS str)) }

cloneMetaTyVarName :: Name -> TcM Name
cloneMetaTyVarName name
  = do { uniq <- newUnique
       ; return (mkSystemName uniq (nameOccName name)) }
         -- See Note [Name of an instantiated type variable]

{- Note [Name of an instantiated type variable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At the moment we give a unification variable a System Name, which
influences the way it is tidied; see TypeRep.tidyTyVarBndr.
-}

metaInfoToTyVarName :: MetaInfo -> FastString
metaInfoToTyVarName  meta_info =
  case meta_info of
       TauTv          -> fsLit "t"
       TyVarTv        -> fsLit "a"
       RuntimeUnkTv   -> fsLit "r"
       CycleBreakerTv -> fsLit "b"

newAnonMetaTyVar :: MetaInfo -> Kind -> TcM TcTyVar
newAnonMetaTyVar mi = newNamedAnonMetaTyVar (metaInfoToTyVarName mi) mi

newNamedAnonMetaTyVar :: FastString -> MetaInfo -> Kind -> TcM TcTyVar
-- Make a new meta tyvar out of thin air
newNamedAnonMetaTyVar tyvar_name meta_info kind

  = do  { name    <- newMetaTyVarName tyvar_name
        ; details <- newMetaDetails meta_info
        ; let tyvar = mkTcTyVar name kind details
        ; traceTc "newAnonMetaTyVar" (ppr tyvar)
        ; return tyvar }

-- makes a new skolem tv
newSkolemTyVar :: Name -> Kind -> TcM TcTyVar
newSkolemTyVar name kind
  = do { lvl <- getTcLevel
       ; return (mkTcTyVar name kind (SkolemTv lvl False)) }

newTyVarTyVar :: Name -> Kind -> TcM TcTyVar
-- See Note [TyVarTv]
-- Does not clone a fresh unique
newTyVarTyVar name kind
  = do { details <- newMetaDetails TyVarTv
       ; let tyvar = mkTcTyVar name kind details
       ; traceTc "newTyVarTyVar" (ppr tyvar)
       ; return tyvar }

cloneTyVarTyVar :: Name -> Kind -> TcM TcTyVar
-- See Note [TyVarTv]
-- Clones a fresh unique
cloneTyVarTyVar name kind
  = do { details <- newMetaDetails TyVarTv
       ; uniq <- newUnique
       ; let name' = name `setNameUnique` uniq
             tyvar = mkTcTyVar name' kind details
         -- Don't use cloneMetaTyVar, which makes a SystemName
         -- We want to keep the original more user-friendly Name
         -- In practical terms that means that in error messages,
         -- when the Name is tidied we get 'a' rather than 'a0'
       ; traceTc "cloneTyVarTyVar" (ppr tyvar)
       ; return tyvar }

newPatSigTyVar :: Name -> Kind -> TcM TcTyVar
newPatSigTyVar name kind
  = do { details <- newMetaDetails TauTv
       ; uniq <- newUnique
       ; let name' = name `setNameUnique` uniq
             tyvar = mkTcTyVar name' kind details
         -- Don't use cloneMetaTyVar;
         -- same reasoning as in newTyVarTyVar
       ; traceTc "newPatSigTyVar" (ppr tyvar)
       ; return tyvar }

cloneAnonMetaTyVar :: MetaInfo -> TyVar -> TcKind -> TcM TcTyVar
-- Make a fresh MetaTyVar, basing the name
-- on that of the supplied TyVar
cloneAnonMetaTyVar info tv kind
  = do  { details <- newMetaDetails info
        ; name    <- cloneMetaTyVarName (tyVarName tv)
        ; let tyvar = mkTcTyVar name kind details
        ; traceTc "cloneAnonMetaTyVar" (ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar))
        ; return tyvar }

-- Make a new CycleBreakerTv. See Note [Type variable cycles in Givens]
-- in GHC.Tc.Solver.Canonical.
newCycleBreakerTyVar :: TcKind -> TcM TcTyVar
newCycleBreakerTyVar kind
  = do { details <- newMetaDetails CycleBreakerTv
       ; name <- newMetaTyVarName (fsLit "cbv")
       ; return (mkTcTyVar name kind details) }

newMetaDetails :: MetaInfo -> TcM TcTyVarDetails
newMetaDetails info
  = do { ref <- newMutVar Flexi
       ; tclvl <- getTcLevel
       ; return (MetaTv { mtv_info = info
                        , mtv_ref = ref
                        , mtv_tclvl = tclvl }) }

newTauTvDetailsAtLevel :: TcLevel -> TcM TcTyVarDetails
newTauTvDetailsAtLevel tclvl
  = do { ref <- newMutVar Flexi
       ; return (MetaTv { mtv_info  = TauTv
                        , mtv_ref   = ref
                        , mtv_tclvl = tclvl }) }

cloneMetaTyVar :: TcTyVar -> TcM TcTyVar
cloneMetaTyVar tv
  = ASSERT( isTcTyVar tv )
    do  { ref  <- newMutVar Flexi
        ; name' <- cloneMetaTyVarName (tyVarName tv)
        ; let details' = case tcTyVarDetails tv of
                           details@(MetaTv {}) -> details { mtv_ref = ref }
                           _ -> pprPanic "cloneMetaTyVar" (ppr tv)
              tyvar = mkTcTyVar name' (tyVarKind tv) details'
        ; traceTc "cloneMetaTyVar" (ppr tyvar)
        ; return tyvar }

-- Works for both type and kind variables
readMetaTyVar :: TyVar -> TcM MetaDetails
readMetaTyVar tyvar = ASSERT2( isMetaTyVar tyvar, ppr tyvar )
                      readMutVar (metaTyVarRef tyvar)

isFilledMetaTyVar_maybe :: TcTyVar -> TcM (Maybe Type)
isFilledMetaTyVar_maybe tv
 | MetaTv { mtv_ref = ref } <- tcTyVarDetails tv
 = do { cts <- readTcRef ref
      ; case cts of
          Indirect ty -> return (Just ty)
          Flexi       -> return Nothing }
 | otherwise
 = return Nothing

isFilledMetaTyVar :: TyVar -> TcM Bool
-- True of a filled-in (Indirect) meta type variable
isFilledMetaTyVar tv = isJust <$> isFilledMetaTyVar_maybe tv

isUnfilledMetaTyVar :: TyVar -> TcM Bool
-- True of a un-filled-in (Flexi) meta type variable
-- NB: Not the opposite of isFilledMetaTyVar
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
  = ASSERT2( False, text "Writing to non-tc tyvar" <+> ppr tyvar )
    return ()

  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tyvar
  = writeMetaTyVarRef tyvar ref ty

  | otherwise
  = ASSERT2( False, text "Writing to non-meta tyvar" <+> ppr tyvar )
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
  -- Need to zonk 'ty' because we may only recently have promoted
  -- its free meta-tyvars (see Solver.Interact.tryToSolveByUnification)
  | otherwise
  = do { meta_details <- readMutVar ref;
       -- Zonk kinds to allow the error check to work
       ; zonked_tv_kind <- zonkTcType tv_kind
       ; zonked_ty      <- zonkTcType ty
       ; let zonked_ty_kind = tcTypeKind zonked_ty
             zonked_ty_lvl  = tcTypeLevel zonked_ty
             level_check_ok  = not (zonked_ty_lvl `strictlyDeeperThan` tv_lvl)
             level_check_msg = ppr zonked_ty_lvl $$ ppr tv_lvl $$ ppr tyvar $$ ppr ty
             kind_check_ok = tcIsConstraintKind zonked_tv_kind
                          || tcEqKind zonked_ty_kind zonked_tv_kind
             -- Hack alert! tcIsConstraintKind: see GHC.Tc.Gen.HsType
             -- Note [Extra-constraint holes in partial type signatures]

             kind_msg = hang (text "Ill-kinded update to meta tyvar")
                           2 (    ppr tyvar <+> text "::" <+> (ppr tv_kind $$ ppr zonked_tv_kind)
                              <+> text ":="
                              <+> ppr ty <+> text "::" <+> (ppr zonked_ty_kind) )

       ; traceTc "writeMetaTyVar" (ppr tyvar <+> text ":=" <+> ppr ty)

       -- Check for double updates
       ; MASSERT2( isFlexi meta_details, double_upd_msg meta_details )

       -- Check for level OK
       ; MASSERT2( level_check_ok, level_check_msg )

       -- Check Kinds ok
       ; MASSERT2( kind_check_ok, kind_msg )

       -- Do the write
       ; writeMutVar ref (Indirect ty) }
  where
    tv_kind = tyVarKind tyvar

    tv_lvl = tcTyVarLevel tyvar


    double_upd_msg details = hang (text "Double update of meta tyvar")
                                2 (ppr tyvar $$ ppr details)

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


newMultiplicityVar :: TcM TcType
newMultiplicityVar = newFlexiTyVarTy multiplicityTy

newFlexiTyVar :: Kind -> TcM TcTyVar
newFlexiTyVar kind = newAnonMetaTyVar TauTv kind

-- | Create a new flexi ty var with a specific name
newNamedFlexiTyVar :: FastString -> Kind -> TcM TcTyVar
newNamedFlexiTyVar fs kind = newNamedAnonMetaTyVar fs TauTv kind

newFlexiTyVarTy :: Kind -> TcM TcType
newFlexiTyVarTy kind = do
    tc_tyvar <- newFlexiTyVar kind
    return (mkTyVarTy tc_tyvar)

newFlexiTyVarTys :: Int -> Kind -> TcM [TcType]
newFlexiTyVarTys n kind = replicateM n (newFlexiTyVarTy kind)

newOpenTypeKind :: TcM TcKind
newOpenTypeKind
  = do { rr <- newFlexiTyVarTy runtimeRepTy
       ; return (tYPE rr) }

-- | Create a tyvar that can be a lifted or unlifted type.
-- Returns alpha :: TYPE kappa, where both alpha and kappa are fresh
newOpenFlexiTyVarTy :: TcM TcType
newOpenFlexiTyVarTy
  = do { tv <- newOpenFlexiTyVar
       ; return (mkTyVarTy tv) }

newOpenFlexiTyVar :: TcM TcTyVar
newOpenFlexiTyVar
  = do { kind <- newOpenTypeKind
       ; newFlexiTyVar kind }

newMetaTyVars :: [TyVar] -> TcM (TCvSubst, [TcTyVar])
-- Instantiate with META type variables
-- Note that this works for a sequence of kind, type, and coercion variables
-- variables.  Eg    [ (k:*), (a:k->k) ]
--             Gives [ (k7:*), (a8:k7->k7) ]
newMetaTyVars = newMetaTyVarsX emptyTCvSubst
    -- emptyTCvSubst has an empty in-scope set, but that's fine here
    -- Since the tyvars are freshly made, they cannot possibly be
    -- captured by any existing for-alls.

newMetaTyVarsX :: TCvSubst -> [TyVar] -> TcM (TCvSubst, [TcTyVar])
-- Just like newMetaTyVars, but start with an existing substitution.
newMetaTyVarsX subst = mapAccumLM newMetaTyVarX subst

newMetaTyVarX :: TCvSubst -> TyVar -> TcM (TCvSubst, TcTyVar)
-- Make a new unification variable tyvar whose Name and Kind come from
-- an existing TyVar. We substitute kind variables in the kind.
newMetaTyVarX = new_meta_tv_x TauTv

newMetaTyVarTyVarX :: TCvSubst -> TyVar -> TcM (TCvSubst, TcTyVar)
-- Just like newMetaTyVarX, but make a TyVarTv
newMetaTyVarTyVarX = new_meta_tv_x TyVarTv

newWildCardX :: TCvSubst -> TyVar -> TcM (TCvSubst, TcTyVar)
newWildCardX subst tv
  = do { new_tv <- newAnonMetaTyVar TauTv (substTy subst (tyVarKind tv))
       ; return (extendTvSubstWithClone subst tv new_tv, new_tv) }

new_meta_tv_x :: MetaInfo -> TCvSubst -> TyVar -> TcM (TCvSubst, TcTyVar)
new_meta_tv_x info subst tv
  = do  { new_tv <- cloneAnonMetaTyVar info tv substd_kind
        ; let subst1 = extendTvSubstWithClone subst tv new_tv
        ; return (subst1, new_tv) }
  where
    substd_kind = substTyUnchecked subst (tyVarKind tv)
      -- NOTE: #12549 is fixed so we could use
      -- substTy here, but the tc_infer_args problem
      -- is not yet fixed so leaving as unchecked for now.
      -- OLD NOTE:
      -- Unchecked because we call newMetaTyVarX from
      -- tcInstTyBinder, which is called from tcInferTyApps
      -- which does not yet take enough trouble to ensure
      -- the in-scope set is right; e.g. #12785 trips
      -- if we use substTy here

newMetaTyVarTyAtLevel :: TcLevel -> TcKind -> TcM TcType
newMetaTyVarTyAtLevel tc_lvl kind
  = do  { details <- newTauTvDetailsAtLevel tc_lvl
        ; name    <- newMetaTyVarName (fsLit "p")
        ; return (mkTyVarTy (mkTcTyVar name kind details)) }

{- *********************************************************************
*                                                                      *
          Finding variables to quantify over
*                                                                      *
********************************************************************* -}

{- Note [Dependent type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Haskell type inference we quantify over type variables; but we only
quantify over /kind/ variables when -XPolyKinds is on.  Without -XPolyKinds
we default the kind variables to *.

So, to support this defaulting, and only for that reason, when
collecting the free vars of a type (in candidateQTyVarsOfType and friends),
prior to quantifying, we must keep the type and kind variables separate.

But what does that mean in a system where kind variables /are/ type
variables? It's a fairly arbitrary distinction based on how the
variables appear:

  - "Kind variables" appear in the kind of some other free variable
    or in the kind of a locally quantified type variable
    (forall (a :: kappa). ...) or in the kind of a coercion
    (a |> (co :: kappa1 ~ kappa2)).

     These are the ones we default to * if -XPolyKinds is off

  - "Type variables" are all free vars that are not kind variables

E.g.  In the type    T k (a::k)
      'k' is a kind variable, because it occurs in the kind of 'a',
          even though it also appears at "top level" of the type
      'a' is a type variable, because it doesn't

We gather these variables using a CandidatesQTvs record:
  DV { dv_kvs: Variables free in the kind of a free type variable
               or of a forall-bound type variable
     , dv_tvs: Variables syntactically free in the type }

So:  dv_kvs            are the kind variables of the type
     (dv_tvs - dv_kvs) are the type variable of the type

Note that

* A variable can occur in both.
      T k (x::k)    The first occurrence of k makes it
                    show up in dv_tvs, the second in dv_kvs

* We include any coercion variables in the "dependent",
  "kind-variable" set because we never quantify over them.

* The "kind variables" might depend on each other; e.g
     (k1 :: k2), (k2 :: *)
  The "type variables" do not depend on each other; if
  one did, it'd be classified as a kind variable!

Note [CandidatesQTvs determinism and order]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Determinism: when we quantify over type variables we decide the
  order in which they appear in the final type. Because the order of
  type variables in the type can end up in the interface file and
  affects some optimizations like worker-wrapper, we want this order to
  be deterministic.

  To achieve that we use deterministic sets of variables that can be
  converted to lists in a deterministic order. For more information
  about deterministic sets see Note [Deterministic UniqFM] in GHC.Types.Unique.DFM.

* Order: as well as being deterministic, we use an
  accumulating-parameter style for candidateQTyVarsOfType so that we
  add variables one at a time, left to right.  That means we tend to
  produce the variables in left-to-right order.  This is just to make
  it bit more predictable for the programmer.

Note [Naughty quantification candidates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#14880, dependent/should_compile/T14880-2), suppose
we are trying to generalise this type:

  forall arg. ... (alpha[tau]:arg) ...

We have a metavariable alpha whose kind mentions a skolem variable
bound inside the very type we are generalising.
This can arise while type-checking a user-written type signature
(see the test case for the full code).

We cannot generalise over alpha!  That would produce a type like
  forall {a :: arg}. forall arg. ...blah...
The fact that alpha's kind mentions arg renders it completely
ineligible for generalisation.

However, we are not going to learn any new constraints on alpha,
because its kind isn't even in scope in the outer context (but see Wrinkle).
So alpha is entirely unconstrained.

What then should we do with alpha?  During generalization, every
metavariable is either (A) promoted, (B) generalized, or (C) zapped
(according to Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType).

 * We can't generalise it.
 * We can't promote it, because its kind prevents that
 * We can't simply leave it be, because this type is about to
   go into the typing environment (as the type of some let-bound
   variable, say), and then chaos erupts when we try to instantiate.

Previously, we zapped it to Any. This worked, but it had the unfortunate
effect of causing Any sometimes to appear in error messages. If this
kind of signature happens, the user probably has made a mistake -- no
one really wants Any in their types. So we now error. This must be
a hard error (failure in the monad) to avoid other messages from mentioning
Any.

We do this eager erroring in candidateQTyVars, which always precedes
generalisation, because at that moment we have a clear picture of what
skolems are in scope within the type itself (e.g. that 'forall arg').

This change is inspired by and described in Section 7.2 of "Kind Inference
for Datatypes", POPL'20.

Wrinkle:

We must make absolutely sure that alpha indeed is not
from an outer context. (Otherwise, we might indeed learn more information
about it.) This can be done easily: we just check alpha's TcLevel.
That level must be strictly greater than the ambient TcLevel in order
to treat it as naughty. We say "strictly greater than" because the call to
candidateQTyVars is made outside the bumped TcLevel, as stated in the
comment to candidateQTyVarsOfType. The level check is done in go_tv
in collect_cand_qtvs. Skipping this check caused #16517.

-}

data CandidatesQTvs
  -- See Note [Dependent type variables]
  -- See Note [CandidatesQTvs determinism and order]
  --
  -- Invariants:
  --   * All variables are fully zonked, including their kinds
  --   * All variables are at a level greater than the ambient level
  --     See Note [Use level numbers for quantification]
  --
  -- This *can* contain skolems. For example, in `data X k :: k -> Type`
  -- we need to know that the k is a dependent variable. This is done
  -- by collecting the candidates in the kind after skolemising. It also
  -- comes up when generalizing a associated type instance, where instance
  -- variables are skolems. (Recall that associated type instances are generalized
  -- independently from their enclosing class instance, and the associated
  -- type instance may be generalized by more, fewer, or different variables
  -- than the class instance.)
  --
  = DV { dv_kvs :: DTyVarSet    -- "kind" metavariables (dependent)
       , dv_tvs :: DTyVarSet    -- "type" metavariables (non-dependent)
         -- A variable may appear in both sets
         -- E.g.   T k (x::k)    The first occurrence of k makes it
         --                      show up in dv_tvs, the second in dv_kvs
         -- See Note [Dependent type variables]

       , dv_cvs :: CoVarSet
         -- These are covars. Included only so that we don't repeatedly
         -- look at covars' kinds in accumulator. Not used by quantifyTyVars.
    }

instance Semi.Semigroup CandidatesQTvs where
   (DV { dv_kvs = kv1, dv_tvs = tv1, dv_cvs = cv1 })
     <> (DV { dv_kvs = kv2, dv_tvs = tv2, dv_cvs = cv2 })
          = DV { dv_kvs = kv1 `unionDVarSet` kv2
               , dv_tvs = tv1 `unionDVarSet` tv2
               , dv_cvs = cv1 `unionVarSet` cv2 }

instance Monoid CandidatesQTvs where
   mempty = DV { dv_kvs = emptyDVarSet, dv_tvs = emptyDVarSet, dv_cvs = emptyVarSet }
   mappend = (Semi.<>)

instance Outputable CandidatesQTvs where
  ppr (DV {dv_kvs = kvs, dv_tvs = tvs, dv_cvs = cvs })
    = text "DV" <+> braces (pprWithCommas id [ text "dv_kvs =" <+> ppr kvs
                                             , text "dv_tvs =" <+> ppr tvs
                                             , text "dv_cvs =" <+> ppr cvs ])


candidateKindVars :: CandidatesQTvs -> TyVarSet
candidateKindVars dvs = dVarSetToVarSet (dv_kvs dvs)

partitionCandidates :: CandidatesQTvs -> (TyVar -> Bool) -> (TyVarSet, CandidatesQTvs)
-- The selected TyVars are returned as a non-deterministic TyVarSet
partitionCandidates dvs@(DV { dv_kvs = kvs, dv_tvs = tvs }) pred
  = (extracted, dvs { dv_kvs = rest_kvs, dv_tvs = rest_tvs })
  where
    (extracted_kvs, rest_kvs) = partitionDVarSet pred kvs
    (extracted_tvs, rest_tvs) = partitionDVarSet pred tvs
    extracted = dVarSetToVarSet extracted_kvs `unionVarSet` dVarSetToVarSet extracted_tvs

-- | Gathers free variables to use as quantification candidates (in
-- 'quantifyTyVars'). This might output the same var
-- in both sets, if it's used in both a type and a kind.
-- The variables to quantify must have a TcLevel strictly greater than
-- the ambient level. (See Wrinkle in Note [Naughty quantification candidates])
-- See Note [CandidatesQTvs determinism and order]
-- See Note [Dependent type variables]
candidateQTyVarsOfType :: TcType       -- not necessarily zonked
                       -> TcM CandidatesQTvs
candidateQTyVarsOfType ty = collect_cand_qtvs ty False emptyVarSet mempty ty

-- | Like 'candidateQTyVarsOfType', but over a list of types
-- The variables to quantify must have a TcLevel strictly greater than
-- the ambient level. (See Wrinkle in Note [Naughty quantification candidates])
candidateQTyVarsOfTypes :: [Type] -> TcM CandidatesQTvs
candidateQTyVarsOfTypes tys = foldlM (\acc ty -> collect_cand_qtvs ty False emptyVarSet acc ty)
                                     mempty tys

-- | Like 'candidateQTyVarsOfType', but consider every free variable
-- to be dependent. This is appropriate when generalizing a *kind*,
-- instead of a type. (That way, -XNoPolyKinds will default the variables
-- to Type.)
candidateQTyVarsOfKind :: TcKind       -- Not necessarily zonked
                       -> TcM CandidatesQTvs
candidateQTyVarsOfKind ty = collect_cand_qtvs ty True emptyVarSet mempty ty

candidateQTyVarsOfKinds :: [TcKind]    -- Not necessarily zonked
                       -> TcM CandidatesQTvs
candidateQTyVarsOfKinds tys = foldM (\acc ty -> collect_cand_qtvs ty True emptyVarSet acc ty)
                                    mempty tys

delCandidates :: CandidatesQTvs -> [Var] -> CandidatesQTvs
delCandidates (DV { dv_kvs = kvs, dv_tvs = tvs, dv_cvs = cvs }) vars
  = DV { dv_kvs = kvs `delDVarSetList` vars
       , dv_tvs = tvs `delDVarSetList` vars
       , dv_cvs = cvs `delVarSetList`  vars }

collect_cand_qtvs
  :: TcType          -- original type that we started recurring into; for errors
  -> Bool            -- True <=> consider every fv in Type to be dependent
  -> VarSet          -- Bound variables (locals only)
  -> CandidatesQTvs  -- Accumulating parameter
  -> Type            -- Not necessarily zonked
  -> TcM CandidatesQTvs

-- Key points:
--   * Looks through meta-tyvars as it goes;
--     no need to zonk in advance
--
--   * Needs to be monadic anyway, because it handles naughty
--     quantification; see Note [Naughty quantification candidates]
--
--   * Returns fully-zonked CandidateQTvs, including their kinds
--     so that subsequent dependency analysis (to build a well
--     scoped telescope) works correctly

collect_cand_qtvs orig_ty is_dep bound dvs ty
  = go dvs ty
  where
    is_bound tv = tv `elemVarSet` bound

    -----------------
    go :: CandidatesQTvs -> TcType -> TcM CandidatesQTvs
    -- Uses accumulating-parameter style
    go dv (AppTy t1 t2)    = foldlM go dv [t1, t2]
    go dv (TyConApp _ tys) = foldlM go dv tys
    go dv (FunTy _ w arg res) = foldlM go dv [w, arg, res]
    go dv (LitTy {})        = return dv
    go dv (CastTy ty co)    = do dv1 <- go dv ty
                                 collect_cand_qtvs_co orig_ty bound dv1 co
    go dv (CoercionTy co)   = collect_cand_qtvs_co orig_ty bound dv co

    go dv (TyVarTy tv)
      | is_bound tv = return dv
      | otherwise   = do { m_contents <- isFilledMetaTyVar_maybe tv
                         ; case m_contents of
                             Just ind_ty -> go dv ind_ty
                             Nothing     -> go_tv dv tv }

    go dv (ForAllTy (Bndr tv _) ty)
      = do { dv1 <- collect_cand_qtvs orig_ty True bound dv (tyVarKind tv)
           ; collect_cand_qtvs orig_ty is_dep (bound `extendVarSet` tv) dv1 ty }

    -----------------
    go_tv dv@(DV { dv_kvs = kvs, dv_tvs = tvs }) tv
      | tv `elemDVarSet` kvs
      = return dv  -- We have met this tyvar already

      | not is_dep
      , tv `elemDVarSet` tvs
      = return dv  -- We have met this tyvar already

      | otherwise
      = do { tv_kind <- zonkTcType (tyVarKind tv)
                 -- This zonk is annoying, but it is necessary, both to
                 -- ensure that the collected candidates have zonked kinds
                 -- (#15795) and to make the naughty check
                 -- (which comes next) works correctly

           ; let tv_kind_vars = tyCoVarsOfType tv_kind
           ; cur_lvl <- getTcLevel
           ; if |  tcTyVarLevel tv <= cur_lvl
                -> return dv   -- this variable is from an outer context; skip
                               -- See Note [Use level numbers ofor quantification]

                |  intersectsVarSet bound tv_kind_vars
                   -- the tyvar must not be from an outer context, but we have
                   -- already checked for this.
                   -- See Note [Naughty quantification candidates]
                -> do { traceTc "Naughty quantifier" $
                          vcat [ ppr tv <+> dcolon <+> ppr tv_kind
                               , text "bound:" <+> pprTyVars (nonDetEltsUniqSet bound)
                               , text "fvs:" <+> pprTyVars (nonDetEltsUniqSet tv_kind_vars) ]

                      ; let escapees = intersectVarSet bound tv_kind_vars
                      ; naughtyQuantification orig_ty tv escapees }

                |  otherwise
                -> do { let tv' = tv `setTyVarKind` tv_kind
                            dv' | is_dep    = dv { dv_kvs = kvs `extendDVarSet` tv' }
                                | otherwise = dv { dv_tvs = tvs `extendDVarSet` tv' }
                                -- See Note [Order of accumulation]

                        -- See Note [Recurring into kinds for candidateQTyVars]
                      ; collect_cand_qtvs orig_ty True bound dv' tv_kind } }

collect_cand_qtvs_co :: TcType -- original type at top of recursion; for errors
                     -> VarSet -- bound variables
                     -> CandidatesQTvs -> Coercion
                     -> TcM CandidatesQTvs
collect_cand_qtvs_co orig_ty bound = go_co
  where
    go_co dv (Refl ty)             = collect_cand_qtvs orig_ty True bound dv ty
    go_co dv (GRefl _ ty mco)      = do dv1 <- collect_cand_qtvs orig_ty True bound dv ty
                                        go_mco dv1 mco
    go_co dv (TyConAppCo _ _ cos)  = foldlM go_co dv cos
    go_co dv (AppCo co1 co2)       = foldlM go_co dv [co1, co2]
    go_co dv (FunCo _ w co1 co2)   = foldlM go_co dv [w, co1, co2]
    go_co dv (AxiomInstCo _ _ cos) = foldlM go_co dv cos
    go_co dv (AxiomRuleCo _ cos)   = foldlM go_co dv cos
    go_co dv (UnivCo prov _ t1 t2) = do dv1 <- go_prov dv prov
                                        dv2 <- collect_cand_qtvs orig_ty True bound dv1 t1
                                        collect_cand_qtvs orig_ty True bound dv2 t2
    go_co dv (SymCo co)            = go_co dv co
    go_co dv (TransCo co1 co2)     = foldlM go_co dv [co1, co2]
    go_co dv (NthCo _ _ co)        = go_co dv co
    go_co dv (LRCo _ co)           = go_co dv co
    go_co dv (InstCo co1 co2)      = foldlM go_co dv [co1, co2]
    go_co dv (KindCo co)           = go_co dv co
    go_co dv (SubCo co)            = go_co dv co

    go_co dv (HoleCo hole)
      = do m_co <- unpackCoercionHole_maybe hole
           case m_co of
             Just co -> go_co dv co
             Nothing -> go_cv dv (coHoleCoVar hole)

    go_co dv (CoVarCo cv) = go_cv dv cv

    go_co dv (ForAllCo tcv kind_co co)
      = do { dv1 <- go_co dv kind_co
           ; collect_cand_qtvs_co orig_ty (bound `extendVarSet` tcv) dv1 co }

    go_mco dv MRefl    = return dv
    go_mco dv (MCo co) = go_co dv co

    go_prov dv (PhantomProv co)    = go_co dv co
    go_prov dv (ProofIrrelProv co) = go_co dv co
    go_prov dv (PluginProv _)      = return dv

    go_cv :: CandidatesQTvs -> CoVar -> TcM CandidatesQTvs
    go_cv dv@(DV { dv_cvs = cvs }) cv
      | is_bound cv         = return dv
      | cv `elemVarSet` cvs = return dv

        -- See Note [Recurring into kinds for candidateQTyVars]
      | otherwise           = collect_cand_qtvs orig_ty True bound
                                    (dv { dv_cvs = cvs `extendVarSet` cv })
                                    (idType cv)

    is_bound tv = tv `elemVarSet` bound

{- Note [Order of accumulation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You might be tempted (like I was) to use unitDVarSet and mappend
rather than extendDVarSet.  However, the union algorithm for
deterministic sets depends on (roughly) the size of the sets. The
elements from the smaller set end up to the right of the elements from
the larger one. When sets are equal, the left-hand argument to
`mappend` goes to the right of the right-hand argument.

In our case, if we use unitDVarSet and mappend, we learn that the free
variables of (a -> b -> c -> d) are [b, a, c, d], and we then quantify
over them in that order. (The a comes after the b because we union the
singleton sets as ({a} `mappend` {b}), producing {b, a}. Thereafter,
the size criterion works to our advantage.) This is just annoying to
users, so I use `extendDVarSet`, which unambiguously puts the new
element to the right.

Note that the unitDVarSet/mappend implementation would not be wrong
against any specification -- just suboptimal and confounding to users.

Note [Recurring into kinds for candidateQTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
First, read Note [Closing over free variable kinds] in GHC.Core.TyCo.FVs, paying
attention to the end of the Note about using an empty bound set when
traversing a variable's kind.

That Note concludes with the recommendation that we empty out the bound
set when recurring into the kind of a type variable. Yet, we do not do
this here. I have two tasks in order to convince you that this code is
right. First, I must show why it is safe to ignore the reasoning in that
Note. Then, I must show why is is necessary to contradict the reasoning in
that Note.

Why it is safe: There can be no
shadowing in the candidateQ... functions: they work on the output of
type inference, which is seeded by the renamer and its insistence to
use different Uniques for different variables. (In contrast, the Core
functions work on the output of optimizations, which may introduce
shadowing.) Without shadowing, the problem studied by
Note [Closing over free variable kinds] in GHC.Core.TyCo.FVs cannot happen.

Why it is necessary:
Wiping the bound set would be just plain wrong here. Consider

  forall k1 k2 (a :: k1). Proxy k2 (a |> (hole :: k1 ~# k2))

We really don't want to think k1 and k2 are free here. (It's true that we'll
never be able to fill in `hole`, but we don't want to go off the rails just
because we have an insoluble coercion hole.) So: why is it wrong to wipe
the bound variables here but right in Core? Because the final statement
in Note [Closing over free variable kinds] in GHC.Core.TyCo.FVs is wrong: not
every variable is either free or bound. A variable can be a hole, too!
The reasoning in that Note then breaks down.

And the reasoning applies just as well to free non-hole variables, so we
retain the bound set always.

-}

{- *********************************************************************
*                                                                      *
             Quantification
*                                                                      *
************************************************************************

Note [quantifyTyVars]
~~~~~~~~~~~~~~~~~~~~~
quantifyTyVars is given the free vars of a type that we
are about to wrap in a forall.

It takes these free type/kind variables (partitioned into dependent and
non-dependent variables) skolemises metavariables with a TcLevel greater
than the ambient level (see Note [Use level numbers of quantification]).

* This function distinguishes between dependent and non-dependent
  variables only to keep correct defaulting behavior with -XNoPolyKinds.
  With -XPolyKinds, it treats both classes of variables identically.

* quantifyTyVars never quantifies over
    - a coercion variable (or any tv mentioned in the kind of a covar)
    - a runtime-rep variable

Note [Use level numbers for quantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The level numbers assigned to metavariables are very useful. Not only
do they track touchability (Note [TcLevel invariants] in GHC.Tc.Utils.TcType),
but they also allow us to determine which variables to
generalise. The rule is this:

  When generalising, quantify only metavariables with a TcLevel greater
  than the ambient level.

This works because we bump the level every time we go inside a new
source-level construct. In a traditional generalisation algorithm, we
would gather all free variables that aren't free in an environment.
However, if a variable is in that environment, it will always have a lower
TcLevel: it came from an outer scope. So we can replace the "free in
environment" check with a level-number check.

Here is an example:

  f x = x + (z True)
    where
      z y = x * x

We start by saying (x :: alpha[1]). When inferring the type of z, we'll
quickly discover that z :: alpha[1]. But it would be disastrous to
generalise over alpha in the type of z. So we need to know that alpha
comes from an outer environment. By contrast, the type of y is beta[2],
and we are free to generalise over it. What's the difference between
alpha[1] and beta[2]? Their levels. beta[2] has the right TcLevel for
generalisation, and so we generalise it. alpha[1] does not, and so
we leave it alone.

Note that not *every* variable with a higher level will get
generalised, either due to the monomorphism restriction or other
quirks. See, for example, the code in GHC.Tc.Solver.decideMonoTyVars
and in GHC.Tc.Gen.HsType.kindGeneralizeSome, both of which exclude
certain otherwise-eligible variables from being generalised.

Using level numbers for quantification is implemented in the candidateQTyVars...
functions, by adding only those variables with a level strictly higher than
the ambient level to the set of candidates.

Note [quantifyTyVars determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The results of quantifyTyVars are wrapped in a forall and can end up in the
interface file. One such example is inferred type signatures. They also affect
the results of optimizations, for example worker-wrapper. This means that to
get deterministic builds quantifyTyVars needs to be deterministic.

To achieve this CandidatesQTvs is backed by deterministic sets which allows them
to be later converted to a list in a deterministic order.

For more information about deterministic sets see
Note [Deterministic UniqFM] in GHC.Types.Unique.DFM.
-}

quantifyTyVars :: CandidatesQTvs   -- See Note [Dependent type variables]
                                   -- Already zonked
               -> TcM [TcTyVar]
-- See Note [quantifyTyVars]
-- Can be given a mixture of TcTyVars and TyVars, in the case of
--   associated type declarations. Also accepts covars, but *never* returns any.
-- According to Note [Use level numbers for quantification] and the
-- invariants on CandidateQTvs, we do not have to filter out variables
-- free in the environment here. Just quantify unconditionally, subject
-- to the restrictions in Note [quantifyTyVars].
quantifyTyVars dvs@(DV{ dv_kvs = dep_kv_set, dv_tvs = nondep_tkv_set })
       -- short-circuit common case
  | isEmptyDVarSet dep_kv_set
  , isEmptyDVarSet nondep_tkv_set
  = do { traceTc "quantifyTyVars has nothing to quantify" empty
       ; return [] }

  | otherwise
  = do { traceTc "quantifyTyVars {" (ppr dvs)

       ; let dep_kvs = scopedSort $ dVarSetElems dep_kv_set
              -- scopedSort: put the kind variables into
              --    well-scoped order.
              --    E.g.  [k, (a::k)] not the other way round

             nondep_tvs = dVarSetElems (nondep_tkv_set `minusDVarSet` dep_kv_set)
                 -- See Note [Dependent type variables]
                 -- The `minus` dep_tkvs removes any kind-level vars
                 --    e.g. T k (a::k)   Since k appear in a kind it'll
                 --    be in dv_kvs, and is dependent. So remove it from
                 --    dv_tvs which will also contain k
                 -- NB kinds of tvs are already zonked

             -- In the non-PolyKinds case, default the kind variables
             -- to *, and zonk the tyvars as usual.  Notice that this
             -- may make quantifyTyVars return a shorter list
             -- than it was passed, but that's ok
       ; poly_kinds  <- xoptM LangExt.PolyKinds
       ; dep_kvs'    <- mapMaybeM (zonk_quant (not poly_kinds)) dep_kvs
       ; nondep_tvs' <- mapMaybeM (zonk_quant False)            nondep_tvs
       ; let final_qtvs = dep_kvs' ++ nondep_tvs'
           -- Because of the order, any kind variables
           -- mentioned in the kinds of the nondep_tvs'
           -- now refer to the dep_kvs'

       ; traceTc "quantifyTyVars }"
           (vcat [ text "nondep:"     <+> pprTyVars nondep_tvs
                 , text "dep:"        <+> pprTyVars dep_kvs
                 , text "dep_kvs'"    <+> pprTyVars dep_kvs'
                 , text "nondep_tvs'" <+> pprTyVars nondep_tvs' ])

       -- We should never quantify over coercion variables; check this
       ; let co_vars = filter isCoVar final_qtvs
       ; MASSERT2( null co_vars, ppr co_vars )

       ; return final_qtvs }
  where
    -- zonk_quant returns a tyvar if it should be quantified over;
    -- otherwise, it returns Nothing. The latter case happens for
    --    * Kind variables, with -XNoPolyKinds: don't quantify over these
    --    * RuntimeRep variables: we never quantify over these
    zonk_quant default_kind tkv
      | not (isTyVar tkv)
      = return Nothing   -- this can happen for a covar that's associated with
                         -- a coercion hole. Test case: typecheck/should_compile/T2494

      | not (isTcTyVar tkv)
      = return (Just tkv)  -- For associated types in a class with a standalone
                           -- kind signature, we have the class variables in
                           -- scope, and they are TyVars not TcTyVars
      | otherwise
      = do { deflt_done <- defaultTyVar default_kind tkv
           ; case deflt_done of
               True  -> return Nothing
               False -> do { tv <- skolemiseQuantifiedTyVar tkv
                           ; return (Just tv) } }

isQuantifiableTv :: TcLevel   -- Level of the context, outside the quantification
                 -> TcTyVar
                 -> Bool
isQuantifiableTv outer_tclvl tcv
  | isTcTyVar tcv  -- Might be a CoVar; change this when gather covars separately
  = tcTyVarLevel tcv > outer_tclvl
  | otherwise
  = False

zonkAndSkolemise :: TcTyCoVar -> TcM TcTyCoVar
-- A tyvar binder is never a unification variable (TauTv),
-- rather it is always a skolem. It *might* be a TyVarTv.
-- (Because non-CUSK type declarations use TyVarTvs.)
-- Regardless, it may have a kind that has not yet been zonked,
-- and may include kind unification variables.
zonkAndSkolemise tyvar
  | isTyVarTyVar tyvar
     -- We want to preserve the binding location of the original TyVarTv.
     -- This is important for error messages. If we don't do this, then
     -- we get bad locations in, e.g., typecheck/should_fail/T2688
  = do { zonked_tyvar <- zonkTcTyVarToTyVar tyvar
       ; skolemiseQuantifiedTyVar zonked_tyvar }

  | otherwise
  = ASSERT2( isImmutableTyVar tyvar || isCoVar tyvar, pprTyVar tyvar )
    zonkTyCoVarKind tyvar

skolemiseQuantifiedTyVar :: TcTyVar -> TcM TcTyVar
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

skolemiseQuantifiedTyVar tv
  = case tcTyVarDetails tv of
      SkolemTv {} -> do { kind <- zonkTcType (tyVarKind tv)
                        ; return (setTyVarKind tv kind) }
        -- It might be a skolem type variable,
        -- for example from a user type signature

      MetaTv {} -> skolemiseUnboundMetaTyVar tv

      _other -> pprPanic "skolemiseQuantifiedTyVar" (ppr tv) -- RuntimeUnk

defaultTyVar :: Bool      -- True <=> please default this kind variable to *
             -> TcTyVar   -- If it's a MetaTyVar then it is unbound
             -> TcM Bool  -- True <=> defaulted away altogether

defaultTyVar default_kind tv
  | not (isMetaTyVar tv)
  = return False

  | isTyVarTyVar tv
    -- Do not default TyVarTvs. Doing so would violate the invariants
    -- on TyVarTvs; see Note [Signature skolems] in GHC.Tc.Utils.TcType.
    -- #13343 is an example; #14555 is another
    -- See Note [Inferring kinds for type declarations] in GHC.Tc.TyCl
  = return False


  | isRuntimeRepVar tv  -- Do not quantify over a RuntimeRep var
                        -- unless it is a TyVarTv, handled earlier
  = do { traceTc "Defaulting a RuntimeRep var to LiftedRep" (ppr tv)
       ; writeMetaTyVar tv liftedRepTy
       ; return True }
  | isMultiplicityVar tv
  = do { traceTc "Defaulting a Multiplicty var to Many" (ppr tv)
       ; writeMetaTyVar tv manyDataConTy
       ; return True }

  | default_kind            -- -XNoPolyKinds and this is a kind var
  = default_kind_var tv     -- so default it to * if possible

  | otherwise
  = return False

  where
    default_kind_var :: TyVar -> TcM Bool
       -- defaultKindVar is used exclusively with -XNoPolyKinds
       -- See Note [Defaulting with -XNoPolyKinds]
       -- It takes an (unconstrained) meta tyvar and defaults it.
       -- Works only on vars of type *; for other kinds, it issues an error.
    default_kind_var kv
      | isLiftedTypeKind (tyVarKind kv)
      = do { traceTc "Defaulting a kind var to *" (ppr kv)
           ; writeMetaTyVar kv liftedTypeKind
           ; return True }
      | otherwise
      = do { addErr (vcat [ text "Cannot default kind variable" <+> quotes (ppr kv')
                          , text "of kind:" <+> ppr (tyVarKind kv')
                          , text "Perhaps enable PolyKinds or add a kind signature" ])
           -- We failed to default it, so return False to say so.
           -- Hence, it'll get skolemised.  That might seem odd, but we must either
           -- promote, skolemise, or zap-to-Any, to satisfy GHC.Tc.Gen.HsType
           --    Note [Recipe for checking a signature]
           -- Otherwise we get level-number assertion failures. It doesn't matter much
           -- because we are in an error situation anyway.
           ; return False
        }
      where
        (_, kv') = tidyOpenTyCoVar emptyTidyEnv kv

skolemiseUnboundMetaTyVar :: TcTyVar -> TcM TyVar
-- We have a Meta tyvar with a ref-cell inside it
-- Skolemise it, so that we are totally out of Meta-tyvar-land
-- We create a skolem TcTyVar, not a regular TyVar
--   See Note [Zonking to Skolem]
skolemiseUnboundMetaTyVar tv
  = ASSERT2( isMetaTyVar tv, ppr tv )
    do  { when debugIsOn (check_empty tv)
        ; here <- getSrcSpanM    -- Get the location from "here"
                                 -- ie where we are generalising
        ; kind <- zonkTcType (tyVarKind tv)
        ; let tv_name     = tyVarName tv
              -- See Note [Skolemising and identity]
              final_name | isSystemName tv_name
                         = mkInternalName (nameUnique tv_name)
                                          (nameOccName tv_name) here
                         | otherwise
                         = tv_name
              final_tv = mkTcTyVar final_name kind details

        ; traceTc "Skolemising" (ppr tv <+> text ":=" <+> ppr final_tv)
        ; writeMetaTyVar tv (mkTyVarTy final_tv)
        ; return final_tv }

  where
    details = SkolemTv (metaTyVarTcLevel tv) False
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
out what we do know at the call of `reverse` by instantiating its type with a fresh
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

which is propagated up to the toplevel (see GHC.Tc.Solver.tcSimplifyInferCheck).
In the meantime `a' is zonked and quantified to form `evalRHS's signature.
This has the *side effect* of also zonking the `a' in the deferred equality
(which at this point is being handed around wrapped in an implication
constraint).

Finally, the equality (with the zonked `a') will be handed back to the
simplifier by GHC.Tc.Module.tcRnSrcDecls calling GHC.Tc.Solver.tcSimplifyTop.
If we zonk `a' with a regular type variable, we will have this regular type
variable now floating around in the simplifier, which in many places assumes to
only see proper TcTyVars.

We can avoid this problem by zonking with a skolem TcTyVar.  The
skolem is rigid (which we require for a quantified variable), but is
still a TcTyVar that the simplifier knows how to deal with.

Note [Skolemising and identity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In some places, we make a TyVarTv for a binder. E.g.
    class C a where ...
As Note [Inferring kinds for type declarations] discusses,
we make a TyVarTv for 'a'.  Later we skolemise it, and we'd
like to retain its identity, location info etc.  (If we don't
retain its identity we'll have to do some pointless swizzling;
see GHC.Tc.TyCl.swizzleTcTyConBndrs.  If we retain its identity
but not its location we'll lose the detailed binding site info.

Conclusion: use the Name of the TyVarTv.  But we don't want
to do that when skolemising random unification variables;
there the location we want is the skolemisation site.

Fortunately we can tell the difference: random unification
variables have System Names.  That's why final_name is
set based on the isSystemName test.


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
-}

{- *********************************************************************
*                                                                      *
              Promotion
*                                                                      *
********************************************************************* -}

promoteMetaTyVarTo :: TcLevel -> TcTyVar -> TcM Bool
-- When we float a constraint out of an implication we must restore
-- invariant (WantedInv) in Note [TcLevel invariants] in GHC.Tc.Utils.TcType
-- Return True <=> we did some promotion
-- Also returns either the original tyvar (no promotion) or the new one
-- See Note [Promoting unification variables]
promoteMetaTyVarTo tclvl tv
  | ASSERT2( isMetaTyVar tv, ppr tv )
    tcTyVarLevel tv `strictlyDeeperThan` tclvl
  = do { cloned_tv <- cloneMetaTyVar tv
       ; let rhs_tv = setMetaTyVarTcLevel cloned_tv tclvl
       ; writeMetaTyVar tv (mkTyVarTy rhs_tv)
       ; traceTc "promoteTyVar" (ppr tv <+> text "-->" <+> ppr rhs_tv)
       ; return True }
   | otherwise
   = return False

-- Returns whether or not *any* tyvar is defaulted
promoteTyVarSet :: TcTyVarSet -> TcM Bool
promoteTyVarSet tvs
  = do { tclvl <- getTcLevel
       ; bools <- mapM (promoteMetaTyVarTo tclvl)  $
                  filter isPromotableMetaTyVar $
                  nonDetEltsUniqSet tvs
         -- Non-determinism is OK because order of promotion doesn't matter
       ; return (or bools) }


{- *********************************************************************
*                                                                      *
              Zonking types
*                                                                      *
********************************************************************* -}

zonkTcTypeAndFV :: TcType -> TcM DTyCoVarSet
-- Zonk a type and take its free variables
-- With kind polymorphism it can be essential to zonk *first*
-- so that we find the right set of free variables.  Eg
--    forall k1. forall (a:k2). a
-- where k2:=k1 is in the substitution.  We don't want
-- k2 to look free in this type!
zonkTcTypeAndFV ty
  = tyCoVarsOfTypeDSet <$> zonkTcType ty

zonkTyCoVar :: TyCoVar -> TcM TcType
-- Works on TyVars and TcTyVars
zonkTyCoVar tv | isTcTyVar tv = zonkTcTyVar tv
               | isTyVar   tv = mkTyVarTy <$> zonkTyCoVarKind tv
               | otherwise    = ASSERT2( isCoVar tv, ppr tv )
                                mkCoercionTy . mkCoVarCo <$> zonkTyCoVarKind tv
   -- Hackily, when typechecking type and class decls
   -- we have TyVars in scope added (only) in
   -- GHC.Tc.Gen.HsType.bindTyClTyVars, but it seems
   -- painful to make them into TcTyVars there

zonkTyCoVarsAndFV :: TyCoVarSet -> TcM TyCoVarSet
zonkTyCoVarsAndFV tycovars
  = tyCoVarsOfTypes <$> mapM zonkTyCoVar (nonDetEltsUniqSet tycovars)
  -- It's OK to use nonDetEltsUniqSet here because we immediately forget about
  -- the ordering by turning it into a nondeterministic set and the order
  -- of zonking doesn't matter for determinism.

zonkDTyCoVarSetAndFV :: DTyCoVarSet -> TcM DTyCoVarSet
zonkDTyCoVarSetAndFV tycovars
  = mkDVarSet <$> (zonkTyCoVarsAndFVList $ dVarSetElems tycovars)

-- Takes a list of TyCoVars, zonks them and returns a
-- deterministically ordered list of their free variables.
zonkTyCoVarsAndFVList :: [TyCoVar] -> TcM [TyCoVar]
zonkTyCoVarsAndFVList tycovars
  = tyCoVarsOfTypesList <$> mapM zonkTyCoVar tycovars

zonkTcTyVars :: [TcTyVar] -> TcM [TcType]
zonkTcTyVars tyvars = mapM zonkTcTyVar tyvars

-----------------  Types
zonkTyCoVarKind :: TyCoVar -> TcM TyCoVar
zonkTyCoVarKind tv = do { kind' <- zonkTcType (tyVarKind tv)
                        ; return (setTyVarKind tv kind') }

zonkTyCoVarKindBinder :: (VarBndr TyCoVar fl) -> TcM (VarBndr TyCoVar fl)
zonkTyCoVarKindBinder (Bndr tv fl) = do { kind' <- zonkTcType (tyVarKind tv)
                                        ; return $ Bndr (setTyVarKind tv kind') fl }

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
  = do { skols'  <- mapM zonkTyCoVarKind skols  -- Need to zonk their kinds!
                                                -- as #7230 showed
       ; given'  <- mapM zonkEvVar given
       ; info'   <- zonkSkolemInfo info
       ; wanted' <- zonkWCRec wanted
       ; return (implic { ic_skols  = skols'
                        , ic_given  = given'
                        , ic_wanted = wanted'
                        , ic_info   = info' }) }

zonkEvVar :: EvVar -> TcM EvVar
zonkEvVar var = updateIdTypeAndMultM zonkTcType var


zonkWC :: WantedConstraints -> TcM WantedConstraints
zonkWC wc = zonkWCRec wc

zonkWCRec :: WantedConstraints -> TcM WantedConstraints
zonkWCRec (WC { wc_simple = simple, wc_impl = implic, wc_holes = holes })
  = do { simple' <- zonkSimples simple
       ; implic' <- mapBagM zonkImplication implic
       ; holes'  <- mapBagM zonkHole holes
       ; return (WC { wc_simple = simple', wc_impl = implic', wc_holes = holes' }) }

zonkSimples :: Cts -> TcM Cts
zonkSimples cts = do { cts' <- mapBagM zonkCt cts
                     ; traceTc "zonkSimples done:" (ppr cts')
                     ; return cts' }

zonkHole :: Hole -> TcM Hole
zonkHole hole@(Hole { hole_ty = ty })
  = do { ty' <- zonkTcType ty
       ; return (hole { hole_ty = ty' }) }

{- Note [zonkCt behaviour]
~~~~~~~~~~~~~~~~~~~~~~~~~~
zonkCt tries to maintain the canonical form of a Ct.  For example,
  - a CDictCan should stay a CDictCan;
  - a CIrredCan should stay a CIrredCan with its cc_status flag intact

Why?, for example:
- For CDictCan, the @GHC.Tc.Solver.expandSuperClasses@ step, which runs after the
  simple wanted and plugin loop, looks for @CDictCan@s. If a plugin is in use,
  constraints are zonked before being passed to the plugin. This means if we
  don't preserve a canonical form, @expandSuperClasses@ fails to expand
  superclasses. This is what happened in #11525.

- For CIrredCan we want to see if a constraint is insoluble with insolubleWC

On the other hand, we change CEqCan to CNonCanonical, because of all of
CEqCan's invariants, which can break during zonking. (Example: a ~R alpha, where
we have alpha := N Int, where N is a newtype.) Besides, the constraint
will be canonicalised again, so there is little benefit in keeping the
CEqCan structure.

NB: Constraints are always rewritten etc by the canonicaliser in
@GHC.Tc.Solver.Canonical@ even if they come in as CDictCan. Only canonical constraints that
are actually in the inert set carry all the guarantees. So it is okay if zonkCt
creates e.g. a CDictCan where the cc_tyars are /not/ fully reduced.
-}

zonkCt :: Ct -> TcM Ct
-- See Note [zonkCt behaviour]
zonkCt ct@(CDictCan { cc_ev = ev, cc_tyargs = args })
  = do { ev'   <- zonkCtEvidence ev
       ; args' <- mapM zonkTcType args
       ; return $ ct { cc_ev = ev', cc_tyargs = args' } }

zonkCt (CEqCan { cc_ev = ev })
  = mkNonCanonical <$> zonkCtEvidence ev

zonkCt ct@(CIrredCan { cc_ev = ev }) -- Preserve the cc_status flag
  = do { ev' <- zonkCtEvidence ev
       ; return (ct { cc_ev = ev' }) }

zonkCt ct
  = do { fl' <- zonkCtEvidence (ctEvidence ct)
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

-- For unbound, mutable tyvars, zonkType uses the function given to it
-- For tyvars bound at a for-all, zonkType zonks them to an immutable
--      type variable and zonks the kind too
zonkTcType  :: TcType -> TcM TcType
zonkTcTypes :: [TcType] -> TcM [TcType]
zonkCo      :: Coercion -> TcM Coercion

(zonkTcType, zonkTcTypes, zonkCo, _)
  = mapTyCo zonkTcTypeMapper

-- | A suitable TyCoMapper for zonking a type during type-checking,
-- before all metavars are filled in.
zonkTcTypeMapper :: TyCoMapper () TcM
zonkTcTypeMapper = TyCoMapper
  { tcm_tyvar = const zonkTcTyVar
  , tcm_covar = const (\cv -> mkCoVarCo <$> zonkTyCoVarKind cv)
  , tcm_hole  = hole
  , tcm_tycobinder = \_env tv _vis -> ((), ) <$> zonkTyCoVarKind tv
  , tcm_tycon      = zonkTcTyCon }
  where
    hole :: () -> CoercionHole -> TcM Coercion
    hole _ hole@(CoercionHole { ch_ref = ref, ch_co_var = cv })
      = do { contents <- readTcRef ref
           ; case contents of
               Just co -> do { co' <- zonkCo co
                             ; checkCoercionHole cv co' }
               Nothing -> do { cv' <- zonkCoVar cv
                             ; return $ HoleCo (hole { ch_co_var = cv' }) } }

zonkTcTyCon :: TcTyCon -> TcM TcTyCon
-- Only called on TcTyCons
-- A non-poly TcTyCon may have unification
-- variables that need zonking, but poly ones cannot
zonkTcTyCon tc
 | tcTyConIsPoly tc = return tc
 | otherwise        = do { tck' <- zonkTcType (tyConKind tc)
                         ; return (setTcTyConKind tc tck') }

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
                    Indirect ty -> do { zty <- zonkTcType ty
                                      ; writeTcRef ref (Indirect zty)
                                        -- See Note [Sharing in zonking]
                                      ; return zty } }

  | otherwise -- coercion variable
  = zonk_kind_and_return
  where
    zonk_kind_and_return = do { z_tv <- zonkTyCoVarKind tv
                              ; return (mkTyVarTy z_tv) }

-- Variant that assumes that any result of zonking is still a TyVar.
-- Should be used only on skolems and TyVarTvs
zonkTcTyVarToTyVar :: HasDebugCallStack => TcTyVar -> TcM TcTyVar
zonkTcTyVarToTyVar tv
  = do { ty <- zonkTcTyVar tv
       ; let tv' = case tcGetTyVar_maybe ty of
                     Just tv' -> tv'
                     Nothing  -> pprPanic "zonkTcTyVarToTyVar"
                                          (ppr tv $$ ppr ty)
       ; return tv' }

zonkInvisTVBinder :: VarBndr TcTyVar spec -> TcM (VarBndr TyVar spec)
zonkInvisTVBinder (Bndr tv spec) = do { tv' <- zonkTcTyVarToTyVar tv
                                      ; return (Bndr tv' spec) }

-- zonkId is used *during* typechecking just to zonk the Id's type
zonkId :: TcId -> TcM TcId
zonkId id = Id.updateIdTypeAndMultM zonkTcType id

zonkCoVar :: CoVar -> TcM CoVar
zonkCoVar = zonkId

{- Note [Sharing in zonking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   alpha :-> beta :-> gamma :-> ty
where the ":->" means that the unification variable has been
filled in with Indirect. Then when zonking alpha, it'd be nice
to short-circuit beta too, so we end up with
   alpha :-> zty
   beta  :-> zty
   gamma :-> zty
where zty is the zonked version of ty.  That way, if we come across
beta later, we'll have less work to do.  (And indeed the same for
alpha.)

This is easily achieved: just overwrite (Indirect ty) with (Indirect
zty).  Non-systematic perf comparisons suggest that this is a modest
win.

But c.f Note [Sharing when zonking to Type] in GHC.Tc.Utils.Zonk.

%************************************************************************
%*                                                                      *
                 Tidying
*                                                                      *
************************************************************************
-}

zonkTidyTcType :: TidyEnv -> TcType -> TcM (TidyEnv, TcType)
zonkTidyTcType env ty = do { ty' <- zonkTcType ty
                           ; return (tidyOpenType env ty') }

zonkTidyTcTypes :: TidyEnv -> [TcType] -> TcM (TidyEnv, [TcType])
zonkTidyTcTypes = zonkTidyTcTypes' []
  where zonkTidyTcTypes' zs env [] = return (env, reverse zs)
        zonkTidyTcTypes' zs env (ty:tys)
          = do { (env', ty') <- zonkTidyTcType env ty
               ; zonkTidyTcTypes' (ty':zs) env' tys }

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
zonkTidyOrigin env (FunDepOrigin1 p1 o1 l1 p2 o2 l2)
  = do { (env1, p1') <- zonkTidyTcType env  p1
       ; (env2, p2') <- zonkTidyTcType env1 p2
       ; return (env2, FunDepOrigin1 p1' o1 l1 p2' o2 l2) }
zonkTidyOrigin env (FunDepOrigin2 p1 o1 p2 l2)
  = do { (env1, p1') <- zonkTidyTcType env  p1
       ; (env2, p2') <- zonkTidyTcType env1 p2
       ; (env3, o1') <- zonkTidyOrigin env2 o1
       ; return (env3, FunDepOrigin2 p1' o1' p2' l2) }
zonkTidyOrigin env orig = return (env, orig)

----------------
tidyCt :: TidyEnv -> Ct -> Ct
-- Used only in error reporting
tidyCt env ct
  = ct { cc_ev = tidy_ev (ctEvidence ct) }
  where
    tidy_ev :: CtEvidence -> CtEvidence
     -- NB: we do not tidy the ctev_evar field because we don't
     --     show it in error messages
    tidy_ev ctev = ctev { ctev_pred = tidyType env (ctev_pred ctev) }

tidyHole :: TidyEnv -> Hole -> Hole
tidyHole env h@(Hole { hole_ty = ty }) = h { hole_ty = tidyType env ty }

----------------
tidyEvVar :: TidyEnv -> EvVar -> EvVar
tidyEvVar env var = updateIdTypeAndMult (tidyType env) var

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
-- See Note [SigSkol SkolemInfo] in "GHC.Tc.Types.Origin"
tidySigSkol env cx ty tv_prs
  = SigSkol cx (tidy_ty env ty) tv_prs'
  where
    tv_prs' = mapSnd (tidyTyCoVarOcc env) tv_prs
    inst_env = mkNameEnv tv_prs'

    tidy_ty env (ForAllTy (Bndr tv vis) ty)
      = ForAllTy (Bndr tv' vis) (tidy_ty env' ty)
      where
        (env', tv') = tidy_tv_bndr env tv

    tidy_ty env ty@(FunTy InvisArg w arg res) -- Look under  c => t
      = ty { ft_mult = tidy_ty env w,
             ft_arg = tidyType env arg,
             ft_res = tidy_ty env res }

    tidy_ty env ty = tidyType env ty

    tidy_tv_bndr :: TidyEnv -> TyCoVar -> (TidyEnv, TyCoVar)
    tidy_tv_bndr env@(occ_env, subst) tv
      | Just tv' <- lookupNameEnv inst_env (tyVarName tv)
      = ((occ_env, extendVarEnv subst tv tv'), tv')

      | otherwise
      = tidyVarBndr env tv

-------------------------------------------------------------------------
{-
%************************************************************************
%*                                                                      *
             Levity polymorphism checks
*                                                                       *
*************************************************************************

See Note [Levity polymorphism checking] in GHC.HsToCore.Monad

-}

-- | According to the rules around representation polymorphism
-- (see https://gitlab.haskell.org/ghc/ghc/wikis/no-sub-kinds), no binder
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

  -- See Note [Levity polymorphism checking] in GHC.HsToCore.Monad
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
       2 (vcat [ text "Type:" <+> pprWithTYPE tidy_ty
               , text "Kind:" <+> pprWithTYPE tidy_ki ])
  where
    (tidy_env, tidy_ty) = tidyOpenType emptyTidyEnv ty
    tidy_ki             = tidyType tidy_env (tcTypeKind ty)

{-
%************************************************************************
%*                                                                      *
             Error messages
*                                                                       *
*************************************************************************

-}

-- See Note [Naughty quantification candidates]
naughtyQuantification :: TcType   -- original type user wanted to quantify
                      -> TcTyVar  -- naughty var
                      -> TyVarSet -- skolems that would escape
                      -> TcM a
naughtyQuantification orig_ty tv escapees
  = do { orig_ty1 <- zonkTcType orig_ty  -- in case it's not zonked

       ; escapees' <- mapM zonkTcTyVarToTyVar $
                      nonDetEltsUniqSet escapees
                     -- we'll just be printing, so no harmful non-determinism

       ; let fvs  = tyCoVarsOfTypeWellScoped orig_ty1
             env0 = tidyFreeTyCoVars emptyTidyEnv fvs
             env  = env0 `delTidyEnvList` escapees'
                    -- this avoids gratuitous renaming of the escaped
                    -- variables; very confusing to users!

             orig_ty'   = tidyType env orig_ty1
             ppr_tidied = pprTyVars . map (tidyTyCoVarOcc env)
             doc = pprWithExplicitKindsWhen True $
                   vcat [ sep [ text "Cannot generalise type; skolem" <> plural escapees'
                              , quotes $ ppr_tidied escapees'
                              , text "would escape" <+> itsOrTheir escapees' <+> text "scope"
                              ]
                        , sep [ text "if I tried to quantify"
                              , ppr_tidied [tv]
                              , text "in this type:"
                              ]
                        , nest 2 (pprTidiedType orig_ty')
                        , text "(Indeed, I sometimes struggle even printing this correctly,"
                        , text " due to its ill-scoped nature.)"
                        ]

       ; failWithTcM (env, doc) }
