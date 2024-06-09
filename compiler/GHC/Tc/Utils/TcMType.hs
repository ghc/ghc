{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecursiveDo     #-}
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
  newOpenFlexiFRRTyVar, newOpenFlexiFRRTyVarTy,
  newOpenBoxedTypeKind,
  newMetaKindVar, newMetaKindVars,
  newMetaTyVarTyAtLevel, newConcreteTyVarTyAtLevel, substConcreteTvOrigin,
  newAnonMetaTyVar, newConcreteTyVar,
  cloneMetaTyVar, cloneMetaTyVarWithInfo,
  newCycleBreakerTyVar,

  newMultiplicityVar,
  readMetaTyVar, writeMetaTyVar, writeMetaTyVarRef,
  newTauTvDetailsAtLevel, newMetaDetails, newMetaTyVarName,
  isFilledMetaTyVar_maybe, isFilledMetaTyVar, isUnfilledMetaTyVar,

  --------------------------------
  -- Creating new evidence variables
  newEvVar, newEvVars, newDict,
  newWantedWithLoc, newWanted, newWanteds, cloneWanted, cloneWC, cloneWantedCtEv,
  emitWanted, emitWantedEq, emitWantedEvVar, emitWantedEvVars,
  emitWantedEqs,
  newTcEvBinds, newNoTcEvBinds, addTcEvBind,
  emitNewExprHole,

  newCoercionHole, newCoercionHoleO, newVanillaCoercionHole,
  fillCoercionHole, isFilledCoercionHole,
  unpackCoercionHole, unpackCoercionHole_maybe,
  checkCoercionHole,

  newImplication,

  --------------------------------
  -- Instantiation
  newMetaTyVars, newMetaTyVarX, newMetaTyVarsX, newMetaTyVarBndrsX,
  newMetaTyVarTyVarX,
  newTyVarTyVar, cloneTyVarTyVar,
  newConcreteTyVarX,
  newPatTyVar, newSkolemTyVar, newWildCardX,

  --------------------------------
  -- Expected types
  ExpType(..), ExpSigmaType, ExpRhoType,
  mkCheckExpType, newInferExpType, newInferExpTypeFRR,
  tcInfer, tcInferFRR,
  readExpType, readExpType_maybe, readScaledExpType,
  expTypeToType, scaledExpTypeToType,
  checkingExpType_maybe, checkingExpType,
  inferResultToType, ensureMonoType, promoteTcType,

  --------------------------------
  -- Multiplicity usage environment
  tcCheckUsage,

  ---------------------------------
  -- Promotion, defaulting, skolemisation
  defaultTyVar, promoteMetaTyVarTo, promoteTyVarSet,
  quantifyTyVars, isQuantifiableTv,
  zonkAndSkolemise, skolemiseQuantifiedTyVar,
  doNotQuantifyTyVars,

  candidateQTyVarsOfType,  candidateQTyVarsOfKind,
  candidateQTyVarsOfTypes, candidateQTyVarsOfKinds,
  candidateQTyVarsWithBinders,
  CandidatesQTvs(..), delCandidates,
  candidateKindVars, partitionCandidates,

  ------------------------------
  -- Representation polymorphism
  checkTypeHasFixedRuntimeRep,

  -- * Other HsSyn functions
  mkHsDictLet, mkHsApp,
  mkHsAppTy, mkHsCaseAlt,
  tcShortCutLit, shortCutLit, hsOverLitName,
  conLikeResTy
  ) where

import GHC.Prelude

import GHC.Hs
import GHC.Platform

import GHC.Driver.DynFlags
import qualified GHC.LanguageExtensions as LangExt

import {-# SOURCE #-} GHC.Tc.Utils.Unify( unifyInvisibleType, tcSubMult )
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.CtLoc( CtLoc, ctLocOrigin )
import GHC.Tc.Utils.Monad        -- TcType, amongst others
import GHC.Tc.Utils.TcType
import GHC.Tc.Errors.Types
import GHC.Tc.Zonk.Type
import GHC.Tc.Zonk.TcType

import GHC.Builtin.Names

import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Core.Coercion
import GHC.Core.Class
import GHC.Core.Predicate
import GHC.Core.UsageEnv

import GHC.Types.Var
import GHC.Types.Id as Id
import GHC.Types.Name
import GHC.Types.SourceText
import GHC.Types.Var.Set

import GHC.Builtin.Types
import GHC.Types.Var.Env
import GHC.Types.Unique.Set
import GHC.Types.Basic ( TypeOrKind(..)
                       , NonStandardDefaultingStrategy(..)
                       , DefaultingStrategy(..), defaultNonStandardTyVars )

import GHC.Data.FastString
import GHC.Data.Bag

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)

import Control.Monad
import Data.IORef
import GHC.Data.Maybe
import qualified Data.Semigroup as Semi
import GHC.Types.Name.Reader

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
                 ; return (mkLocalIdOrCoVar name ManyTy ty) }

-- | Create a new Wanted constraint with the given 'CtLoc'.
newWantedWithLoc :: CtLoc -> PredType -> TcM CtEvidence
newWantedWithLoc loc pty
  = do dst <- case classifyPredType pty of
                EqPred {} -> HoleDest  <$> newCoercionHole loc pty
                _         -> EvVarDest <$> newEvVar pty
       return $ CtWanted { ctev_dest      = dst
                         , ctev_pred      = pty
                         , ctev_loc       = loc
                         , ctev_rewriters = emptyRewriterSet }

-- | Create a new Wanted constraint with the given 'CtOrigin', and
-- location information taken from the 'TcM' environment.
newWanted :: CtOrigin -> Maybe TypeOrKind -> PredType -> TcM CtEvidence
-- Deals with both equality and non-equality predicates
newWanted orig t_or_k pty
  = do loc <- getCtLocM orig t_or_k
       newWantedWithLoc loc pty

-- | Create new Wanted constraints with the given 'CtOrigin',
-- and location information taken from the 'TcM' environment.
newWanteds :: CtOrigin -> ThetaType -> TcM [CtEvidence]
newWanteds orig = mapM (newWanted orig Nothing)

----------------------------------------------
-- Cloning constraints
----------------------------------------------

cloneWantedCtEv :: CtEvidence -> TcM CtEvidence
cloneWantedCtEv ctev@(CtWanted { ctev_pred = pty, ctev_dest = HoleDest _, ctev_loc = loc })
  | isEqPrimPred pty
  = do { co_hole <- newCoercionHole loc pty
       ; return (ctev { ctev_dest = HoleDest co_hole }) }
  | otherwise
  = pprPanic "cloneWantedCtEv" (ppr pty)
cloneWantedCtEv ctev = return ctev

cloneWanted :: Ct -> TcM Ct
cloneWanted ct = mkNonCanonical <$> cloneWantedCtEv (ctEvidence ct)

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

emitWantedEqs :: CtOrigin -> [(TcType,TcType)] -> TcM ()
-- Emit some new wanted nominal equalities
emitWantedEqs origin pairs
  | null pairs
  = return ()
  | otherwise
  = mapM_ (uncurry (emitWantedEq origin TypeLevel Nominal)) pairs

-- | Emits a new equality constraint
emitWantedEq :: CtOrigin -> TypeOrKind -> Role -> TcType -> TcType -> TcM Coercion
emitWantedEq origin t_or_k role ty1 ty2
  = do { hole <- newCoercionHoleO origin pty
       ; loc  <- getCtLocM origin (Just t_or_k)
       ; emitSimple $ mkNonCanonical $
         CtWanted { ctev_pred      = pty
                  , ctev_dest      = HoleDest hole
                  , ctev_loc       = loc
                  , ctev_rewriters = emptyRewriterSet }
       ; return (HoleCo hole) }
  where
    pty = mkPrimEqPredRole role ty1 ty2

-- | Creates a new EvVar and immediately emits it as a Wanted.
-- No equality predicates here.
emitWantedEvVar :: CtOrigin -> TcPredType -> TcM EvVar
emitWantedEvVar origin ty
  = do { new_cv <- newEvVar ty
       ; loc <- getCtLocM origin Nothing
       ; let ctev = CtWanted { ctev_pred      = ty
                             , ctev_dest      = EvVarDest new_cv
                             , ctev_loc       = loc
                             , ctev_rewriters = emptyRewriterSet }
       ; emitSimple $ mkNonCanonical ctev
       ; return new_cv }

emitWantedEvVars :: CtOrigin -> [TcPredType] -> TcM [EvVar]
emitWantedEvVars orig = mapM (emitWantedEvVar orig)

-- | Emit a new wanted expression hole
emitNewExprHole :: RdrName         -- of the hole
                -> Type -> TcM HoleExprRef
emitNewExprHole occ ty
  = do { u <- newUnique
       ; ref <- newTcRef (pprPanic "unfilled unbound-variable evidence" (ppr u))
       ; let her = HER ref ty u

       ; loc <- getCtLocM (ExprHoleOrigin (Just occ)) (Just TypeLevel)

       ; let hole = Hole { hole_sort = ExprHole her
                         , hole_occ  = occ
                         , hole_ty   = ty
                         , hole_loc  = loc }
       ; emitHole hole
       ; return her }

newDict :: Class -> [TcType] -> TcM DictId
newDict cls tys
  = do { name <- newSysName (mkDictOcc (getOccName cls))
       ; return (mkLocalId name ManyTy (mkClassPred cls tys)) }

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
       let in_gen_code = lclEnvInGeneratedCode env
       return $
         (implicationPrototype (mkCtLocEnv env))
           { ic_warn_inaccessible = warn_inaccessible && not in_gen_code }

{-
************************************************************************
*                                                                      *
        Coercion holes
*                                                                      *
************************************************************************
-}

newVanillaCoercionHole :: TcPredType -> TcM CoercionHole
newVanillaCoercionHole = new_coercion_hole False

newCoercionHole :: CtLoc -> TcPredType -> TcM CoercionHole
newCoercionHole loc = newCoercionHoleO (ctLocOrigin loc)

newCoercionHoleO :: CtOrigin -> TcPredType -> TcM CoercionHole
newCoercionHoleO (KindEqOrigin {}) pty = new_coercion_hole True pty
newCoercionHoleO _ pty                 = new_coercion_hole False pty

new_coercion_hole :: Bool -> TcPredType -> TcM CoercionHole
new_coercion_hole hetero_kind pred_ty
  = do { co_var <- newEvVar pred_ty
       ; traceTc "New coercion hole:" (ppr co_var <+> dcolon <+> ppr pred_ty)
       ; ref <- newMutVar Nothing
       ; return $ CoercionHole { ch_co_var = co_var, ch_ref = ref
                               , ch_hetero_kind = hetero_kind } }

-- | Put a value in a coercion hole
fillCoercionHole :: CoercionHole -> Coercion -> TcM ()
fillCoercionHole (CoercionHole { ch_ref = ref, ch_co_var = cv }) co = do
  when debugIsOn $ do
    cts <- readTcRef ref
    whenIsJust cts $ \old_co ->
      pprPanic "Filling a filled coercion hole" (ppr cv $$ ppr co $$ ppr old_co)
  traceTc "Filling coercion hole" (ppr cv <+> text ":=" <+> ppr co)
  writeTcRef ref (Just co)


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

Note [FixedRuntimeRep context in ExpType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sometimes, we want to be sure that we fill an ExpType with a type
that has a syntactically fixed RuntimeRep (in the sense of
Note [Fixed RuntimeRep] in GHC.Tc.Utils.Concrete).

Example:

  pattern S a = (a :: (T :: TYPE R))

We have to infer a type for `a` which has a syntactically fixed RuntimeRep.
When it comes time to filling in the inferred type, we do the appropriate
representation-polymorphism check, much like we do a level check
as explained in Note [TcLevel of ExpType].

See test case T21325.
-}

-- actual data definition is in GHC.Tc.Utils.TcType

newInferExpType :: TcM ExpType
newInferExpType = new_inferExpType Nothing

newInferExpTypeFRR :: FixedRuntimeRepContext -> TcM ExpTypeFRR
newInferExpTypeFRR frr_orig
  = do { th_stage <- getStage
       ; if
          -- See [Wrinkle: Typed Template Haskell]
          -- in Note [hasFixedRuntimeRep] in GHC.Tc.Utils.Concrete.
          | Brack _ (TcPending {}) <- th_stage
          -> new_inferExpType Nothing

          | otherwise
          -> new_inferExpType (Just frr_orig) }

new_inferExpType :: Maybe FixedRuntimeRepContext -> TcM ExpType
new_inferExpType mb_frr_orig
  = do { u <- newUnique
       ; tclvl <- getTcLevel
       ; traceTc "newInferExpType" (ppr u <+> ppr tclvl)
       ; ref <- newMutVar Nothing
       ; return (Infer (IR { ir_uniq = u, ir_lvl = tclvl
                           , ir_ref = ref
                           , ir_frr = mb_frr_orig })) }

-- | Extract a type out of an ExpType, if one exists. But one should always
-- exist. Unless you're quite sure you know what you're doing.
readExpType_maybe :: MonadIO m => ExpType -> m (Maybe TcType)
readExpType_maybe (Check ty)                   = return (Just ty)
readExpType_maybe (Infer (IR { ir_ref = ref})) = liftIO $ readIORef ref
{-# INLINEABLE readExpType_maybe #-}

-- | Same as readExpType, but for Scaled ExpTypes
readScaledExpType :: MonadIO m => Scaled ExpType -> m (Scaled Type)
readScaledExpType (Scaled m exp_ty)
  = do { ty <- readExpType exp_ty
       ; return (Scaled m ty) }
{-# INLINEABLE readScaledExpType  #-}

-- | Extract a type out of an ExpType. Otherwise, panics.
readExpType :: MonadIO m => ExpType -> m TcType
readExpType exp_ty
  = do { mb_ty <- readExpType_maybe exp_ty
       ; case mb_ty of
           Just ty -> return ty
           Nothing -> pprPanic "Unknown expected type" (ppr exp_ty) }
{-# INLINEABLE readExpType  #-}

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
                      , ir_ref = ref
                      , ir_frr = mb_frr })
  = do { mb_inferred_ty <- readTcRef ref
       ; tau <- case mb_inferred_ty of
            Just ty -> do { ensureMonoType ty
                            -- See Note [inferResultToType]
                          ; return ty }
            Nothing -> do { tau <- new_meta
                          ; writeMutVar ref (Just tau)
                          ; return tau }
       ; traceTc "Forcing ExpType to be monomorphic:"
                 (ppr u <+> text ":=" <+> ppr tau)
       ; return tau }
  where
    -- See Note [TcLevel of ExpType]
    new_meta = case mb_frr of
      Nothing  ->  do { rr  <- newMetaTyVarTyAtLevel tc_lvl runtimeRepTy
                      ; newMetaTyVarTyAtLevel tc_lvl (mkTYPEapp rr) }
      Just frr -> mdo { rr  <- newConcreteTyVarTyAtLevel conc_orig tc_lvl runtimeRepTy
                      ; tau <- newMetaTyVarTyAtLevel tc_lvl (mkTYPEapp rr)
                      ; let conc_orig = ConcreteFRR $ FixedRuntimeRepOrigin tau frr
                      ; return tau }

{- Note [inferResultToType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
expTypeToType and inferResultType convert an InferResult to a monotype.
It must be a monotype because if the InferResult isn't already filled in,
we fill it in with a unification variable (hence monotype).  So to preserve
order-independence we check for mono-type-ness even if it *is* filled in
already.

See also Note [TcLevel of ExpType] above, and
Note [fillInferResult] in GHC.Tc.Utils.Unify.
-}

-- | Infer a type using a fresh ExpType
-- See also Note [ExpType] in "GHC.Tc.Utils.TcMType"
--
-- Use 'tcInferFRR' if you require the type to have a fixed
-- runtime representation.
tcInfer :: (ExpSigmaType -> TcM a) -> TcM (a, TcSigmaType)
tcInfer = tc_infer Nothing

-- | Like 'tcInfer', except it ensures that the resulting type
-- has a syntactically fixed RuntimeRep as per Note [Fixed RuntimeRep] in
-- GHC.Tc.Utils.Concrete.
tcInferFRR :: FixedRuntimeRepContext -> (ExpSigmaTypeFRR -> TcM a) -> TcM (a, TcSigmaTypeFRR)
tcInferFRR frr_orig = tc_infer (Just frr_orig)

tc_infer :: Maybe FixedRuntimeRepContext -> (ExpSigmaType -> TcM a) -> TcM (a, TcSigmaType)
tc_infer mb_frr tc_check
  = do { res_ty <- new_inferExpType mb_frr
       ; result <- tc_check res_ty
       ; res_ty <- readExpType res_ty
       ; return (result, res_ty) }

{- *********************************************************************
*                                                                      *
              Promoting types
*                                                                      *
********************************************************************* -}

ensureMonoType :: TcType -> TcM ()
-- Assuming that the argument type is of kind (TYPE r),
-- ensure that it is a /monotype/
-- If it is not a monotype we can see right away (since unification
-- variables and type-function applications stand for monotypes), but
-- we emit a Wanted equality just to delay the error message until later
ensureMonoType res_ty
  | isTauTy res_ty   -- isTauTy doesn't need zonking or anything
  = return ()
  | otherwise
  = do { mono_ty <- newOpenFlexiTyVarTy
       ; _co <- unifyInvisibleType res_ty mono_ty
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
         then return (mkNomReflCo ty, ty)
         else promote_it }
  where
    promote_it :: TcM (TcCoercion, TcType)
    promote_it  -- Emit a constraint  (alpha :: TYPE rr) ~ ty
                -- where alpha and rr are fresh and from level dest_lvl
      = do { rr      <- newMetaTyVarTyAtLevel dest_lvl runtimeRepTy
           ; prom_ty <- newMetaTyVarTyAtLevel dest_lvl (mkTYPEapp rr)
           ; co <- unifyInvisibleType ty prom_ty
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
skolems.  They are used in two places:

1. In kind signatures, see GHC.Tc.TyCl
      Note [Inferring kinds for type declarations]
   and Note [Using TyVarTvs for kind-checking GADTs]

2. In partial type signatures.  See GHC.Tc.Types
   Note [Quantified variables in partial type signatures]

Sometimes, they can unify with type variables that the user would
rather keep distinct; see #11203 for an example.  So, any client of
this function needs to either allow the TyVarTvs to unify with each
other or check that they don't. In the case of (1) the check is done
in GHC.Tc.TyCl.swizzleTcTyConBndrs.  In case of (2) it's done by
findDupTyVarTvs in GHC.Tc.Gen.Bind.chooseInferredQuantifiers.

Historical note: Before #15050 this (under the name SigTv) was also
used for ScopedTypeVariables in patterns, to make sure these type
variables only refer to other type variables, but this restriction was
dropped, and ScopedTypeVariables can now refer to full types (GHC
Proposal 29).
-}

newMetaTyVarName :: FastString -> TcM Name
-- Makes a /System/ Name, which is eagerly eliminated by
-- the unifier; see GHC.Tc.Utils.Unify.nicer_to_update_tv1, and
-- GHC.Tc.Solver.Equality.canEqTyVarTyVar (nicer_to_update_tv2)
newMetaTyVarName str
  = newSysName (mkTyVarOccFS str)

cloneMetaTyVarName :: Name -> TcM Name
cloneMetaTyVarName name
  = newSysName (nameOccName name)
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
       ConcreteTv {}  -> fsLit "c"

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
newSkolemTyVar :: SkolemInfo -> Name -> Kind -> TcM TcTyVar
newSkolemTyVar skol_info name kind
  = do { lvl <- getTcLevel
       ; return (mkTcTyVar name kind (SkolemTv skol_info lvl False)) }

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

-- | Create a new metavariable, of the given kind, which can only be unified
-- with a concrete type.
--
-- Invariant: the kind must be concrete, as per Note [ConcreteTv].
-- This is checked with an assertion.
newConcreteTyVar :: HasDebugCallStack => ConcreteTvOrigin
                 -> FastString -> TcKind -> TcM TcTyVar
newConcreteTyVar reason fs kind
  = assertPpr (isConcreteType kind) assert_msg $
  do { th_stage <- getStage
     ; if
        -- See [Wrinkle: Typed Template Haskell]
        -- in Note [hasFixedRuntimeRep] in GHC.Tc.Utils.Concrete.
        | Brack _ (TcPending {}) <- th_stage
        -> newNamedAnonMetaTyVar fs TauTv kind

        | otherwise
        -> newNamedAnonMetaTyVar fs (ConcreteTv reason) kind }
  where
    assert_msg = text "newConcreteTyVar: non-concrete kind" <+> ppr kind

newPatTyVar :: Name -> Kind -> TcM TcTyVar
newPatTyVar name kind
  = do { details <- newMetaDetails TauTv
       ; uniq <- newUnique
       ; let name' = name `setNameUnique` uniq
             tyvar = mkTcTyVar name' kind details
         -- Don't use cloneMetaTyVar;
         -- same reasoning as in newTyVarTyVar
       ; traceTc "newPatTyVar" (ppr tyvar)
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

-- Make a new CycleBreakerTv. See Note [Type equality cycles]
-- in GHC.Tc.Solver.Equality
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

newConcreteTvDetailsAtLevel :: ConcreteTvOrigin -> TcLevel -> TcM TcTyVarDetails
newConcreteTvDetailsAtLevel conc_orig tclvl
  = do { ref <- newMutVar Flexi
       ; return (MetaTv { mtv_info  = ConcreteTv conc_orig
                        , mtv_ref   = ref
                        , mtv_tclvl = tclvl }) }

cloneMetaTyVar :: TcTyVar -> TcM TcTyVar
cloneMetaTyVar tv
  = assert (isTcTyVar tv) $
    do  { ref  <- newMutVar Flexi
        ; name' <- cloneMetaTyVarName (tyVarName tv)
        ; let details' = case tcTyVarDetails tv of
                           details@(MetaTv {}) -> details { mtv_ref = ref }
                           _ -> pprPanic "cloneMetaTyVar" (ppr tv)
              tyvar = mkTcTyVar name' (tyVarKind tv) details'
        ; traceTc "cloneMetaTyVar" (ppr tyvar)
        ; return tyvar }

cloneMetaTyVarWithInfo :: MetaInfo -> TcLevel -> TcTyVar -> TcM TcTyVar
cloneMetaTyVarWithInfo info tc_lvl tv
  = assert (isTcTyVar tv) $
    do  { ref  <- newMutVar Flexi
        ; name' <- cloneMetaTyVarName (tyVarName tv)
        ; let details = MetaTv { mtv_info  = info
                               , mtv_ref   = ref
                               , mtv_tclvl = tc_lvl }
              tyvar = mkTcTyVar name' (tyVarKind tv) details
        ; traceTc "cloneMetaTyVarWithInfo" (ppr tyvar)
        ; return tyvar }

-- Works for both type and kind variables
readMetaTyVar :: MonadIO m => TyVar -> m MetaDetails
readMetaTyVar tyvar = assertPpr (isMetaTyVar tyvar) (ppr tyvar) $
                      liftIO $ readIORef (metaTyVarRef tyvar)
{-# SPECIALISE readMetaTyVar :: TyVar -> TcM MetaDetails #-}
{-# SPECIALISE readMetaTyVar :: TyVar -> ZonkM MetaDetails #-}

isFilledMetaTyVar_maybe :: TcTyVar -> TcM (Maybe Type)
isFilledMetaTyVar_maybe tv
-- TODO: This should be an assertion that tv is definitely a TcTyVar but it fails
-- at the moment (Jan 22)
 | isTcTyVar tv
 , MetaTv { mtv_ref = ref } <- tcTyVarDetails tv
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
       ; return (mkTYPEapp rr) }

-- | Create a tyvar that can be a lifted or unlifted type.
-- Returns @alpha :: TYPE kappa@, where both @alpha@ and @kappa@ are fresh.
--
-- Note: you should use 'newOpenFlexiFRRTyVarTy' if you also need to ensure
-- that the representation is concrete, in the sense of Note [Concrete types]
-- in GHC.Tc.Utils.Concrete.
newOpenFlexiTyVarTy :: TcM TcType
newOpenFlexiTyVarTy
  = do { tv <- newOpenFlexiTyVar
       ; return (mkTyVarTy tv) }

newOpenFlexiTyVar :: TcM TcTyVar
newOpenFlexiTyVar
  = do { kind <- newOpenTypeKind
       ; newFlexiTyVar kind }

-- | Like 'newOpenFlexiTyVar', but ensures the type variable has a
-- syntactically fixed RuntimeRep in the sense of Note [Fixed RuntimeRep]
-- in GHC.Tc.Utils.Concrete.
newOpenFlexiFRRTyVar :: FixedRuntimeRepContext -> TcM TcTyVar
newOpenFlexiFRRTyVar frr_ctxt
  = do { th_stage <- getStage
       ; case th_stage of
          { Brack _ (TcPending {}) -- See [Wrinkle: Typed Template Haskell]
              -> newOpenFlexiTyVar -- in Note [hasFixedRuntimeRep] in GHC.Tc.Utils.Concrete.
          ; _ ->
   mdo { let conc_orig = ConcreteFRR $
                          FixedRuntimeRepOrigin
                            { frr_context = frr_ctxt
                            , frr_type    = mkTyVarTy tv }
        ; rr <- mkTyVarTy <$> newConcreteTyVar conc_orig (fsLit "cx") runtimeRepTy
        ; tv <- newFlexiTyVar (mkTYPEapp rr)
        ; return tv } } }

-- | See 'newOpenFlexiFRRTyVar'.
newOpenFlexiFRRTyVarTy :: FixedRuntimeRepContext -> TcM TcType
newOpenFlexiFRRTyVarTy frr_ctxt
  = do { tv <- newOpenFlexiFRRTyVar frr_ctxt
       ; return (mkTyVarTy tv) }

newOpenBoxedTypeKind :: TcM TcKind
newOpenBoxedTypeKind
  = do { lev <- newFlexiTyVarTy (mkTyConTy levityTyCon)
       ; let rr = mkTyConApp boxedRepDataConTyCon [lev]
       ; return (mkTYPEapp rr) }

newMetaTyVars :: [TyVar] -> TcM (Subst, [TcTyVar])
-- Instantiate with META type variables
-- Note that this works for a sequence of kind, type, and coercion variables
-- variables.  Eg    [ (k:*), (a:k->k) ]
--             Gives [ (k7:*), (a8:k7->k7) ]
newMetaTyVars = newMetaTyVarsX emptySubst
    -- emptySubst has an empty in-scope set, but that's fine here
    -- Since the tyvars are freshly made, they cannot possibly be
    -- captured by any existing for-alls.

newMetaTyVarsX :: Subst -> [TyVar] -> TcM (Subst, [TcTyVar])
-- Just like newMetaTyVars, but start with an existing substitution.
newMetaTyVarsX subst = mapAccumLM newMetaTyVarX subst

newMetaTyVarBndrsX :: Subst -> [VarBndr TyVar vis] -> TcM (Subst, [VarBndr TcTyVar vis])
newMetaTyVarBndrsX subst bndrs = do
  (subst, bndrs') <- newMetaTyVarsX subst (binderVars bndrs)
  pure (subst, zipWith mkForAllTyBinder flags bndrs')
  where
    flags = binderFlags bndrs

newMetaTyVarX :: Subst -> TyVar -> TcM (Subst, TcTyVar)
-- Make a new unification variable tyvar whose Name and Kind come from
-- an existing TyVar. We substitute kind variables in the kind.
newMetaTyVarX = new_meta_tv_x TauTv

-- | Like 'newMetaTyVarX', but for concrete type variables.
newConcreteTyVarX :: ConcreteTvOrigin -> Subst -> TyVar -> TcM (Subst, TcTyVar)
newConcreteTyVarX conc subst tv
  = do { th_stage <- getStage
       ; if
          -- See [Wrinkle: Typed Template Haskell]
          -- in Note [hasFixedRuntimeRep] in GHC.Tc.Utils.Concrete.
          | Brack _ (TcPending {}) <- th_stage
          -> new_meta_tv_x TauTv subst tv
          | otherwise
          -> new_meta_tv_x (ConcreteTv conc) subst tv }

newMetaTyVarTyVarX :: Subst -> TyVar -> TcM (Subst, TcTyVar)
-- Just like newMetaTyVarX, but make a TyVarTv
newMetaTyVarTyVarX subst tv = new_meta_tv_x TyVarTv subst tv

newWildCardX :: Subst -> TyVar -> TcM (Subst, TcTyVar)
newWildCardX subst tv
  = do { new_tv <- newAnonMetaTyVar TauTv (substTy subst (tyVarKind tv))
       ; return (extendTvSubstWithClone subst tv new_tv, new_tv) }

new_meta_tv_x :: MetaInfo -> Subst -> TyVar -> TcM (Subst, TcTyVar)
new_meta_tv_x info subst tv
  = do  { new_tv <- cloneAnonMetaTyVar info tv substd_kind
        ; let subst1 = extendTvSubstWithClone subst tv new_tv
        ; return (subst1, new_tv) }
  where
    substd_kind = substTy subst (tyVarKind tv)

newMetaTyVarTyAtLevel :: TcLevel -> TcKind -> TcM TcType
newMetaTyVarTyAtLevel tc_lvl kind
  = do  { details <- newTauTvDetailsAtLevel tc_lvl
        ; name    <- newMetaTyVarName (fsLit "p")
        ; return (mkTyVarTy (mkTcTyVar name kind details)) }

newConcreteTyVarTyAtLevel :: ConcreteTvOrigin -> TcLevel -> TcKind -> TcM TcType
newConcreteTyVarTyAtLevel conc_orig tc_lvl kind
  = do  { details <- newConcreteTvDetailsAtLevel conc_orig tc_lvl
        ; name    <- newMetaTyVarName (fsLit "c")
        ; return (mkTyVarTy (mkTcTyVar name kind details)) }

{- Note [substConcreteTvOrigin]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To report helpful representation-polymorphism errors to the users, we want to
indicate which type caused the error, instead of simply printing kinds.
For example, in

  coerce :: forall {r} (a :: TYPE r) (b :: TYPE r). Coercible a b => a -> b

  bad :: forall {s} (z :: TYPE s). z -> z
  bad = coerce

we want the error message to say (with additional debug info on type variables
for clarity of this explanation):

  The first argument of 'coerce' does not have a fixed runtime representation.
  Its type is:
    a0[tau] :: TYPE r0[conc]
  Could not unify s[sk] with r0[conc] because the former is not a concrete
  RuntimeRep.

This is more informative than just saying that we could not unify s[sk] with
r0[conc]; it's helpful to users to phrase it in terms of types rather than kinds
whenever possible (especially as the kind variables often have inferred Specificity).

To achieve this, we store the type on which the representation-polymorphism
check is being performed, in the field frr_type of FixedRuntimeRepOrigin.
This is all described in Note [Reporting representation-polymorphism errors] in
GHC.Tc.Types.Origin.

However, we have to be careful in the example above, in which we are
instantiating a built-in representation-polymorphic 'Id'. As described in the
Note [Representation-polymorphism checking built-ins] in GHC.Tc.Utils.Concrete, in such
cases we end up storing types appearing in the original type of the primop,
which means for the situation above with 'coerce' we end up with a ConcreteTvOrigin
which includes type variables bound in the original type of 'coerce':

  FixedRuntimeRepOrigin
    { frr_type = a[tv] :: TYPE r[tv]
    , frr_context = "first argument of coerce" }

When we instantiate 'coerce' in the previous example, we obtain a substitution

  [ r[tv] |-> r0[conc], a |-> a0 :: TYPE r0[conc] ]

which we need to apply to the 'frr_type' field in order for the type variables
in the error message to match up.
This is done by the call to 'substConcreteTvOrigin' in 'instantiateSigma'.

Wrinkle [Extending the substitution]

  In certain cases, we need to extend the substitution we get from 'instantiateSigma'.
  For example, suppose we have:

    bad2 :: forall {s} (z :: TYPE s). z -> z
    bad2 = coerce @z

  Then 'instantiateSigma' will only instantiate the inferred type variable 'r'
  of 'coerce', as it needs to leave 'a' un-instantiated so that the visible
  type application '@z' makes sense. In this case, we end up with a substitution

    subst:                   [ r[tv] |-> r0[conc] ]
    body_ty:                forall (a :: TYPE r[tv]) (b :: TYPE r[tv]). ...
    substTy subst body_ty:  forall (a' :: TYPE r0[conc]) (b' :: TYPE r0[conc]). ...

  Now, we still want a substitution that maps (a :: TYPE r[tv]) to
  (a' :: TYPE r0[conc]) in order to apply it to the 'frr_type', so that we don't
  mention the un-substed (a :: TYPE r[tv]) in the error message.
  To achieve this, we extend the substitution with the outermost quantified type
  variables in the leftover (partially-instantiated) type using 'substTyVarBndrs'
  to get the full substitution which we use in 'substConcreteTvOrigin'.
-}

substConcreteTvOrigin :: Subst -> Type -> ConcreteTvOrigin -> ConcreteTvOrigin
substConcreteTvOrigin subst body_ty (ConcreteFRR frr_orig)
  -- See Note [substConcreteTvOrigin], Wrinkle [Extending the substitution]
  -- for the justification of this extra complication.
  = let subst' = case splitForAllTyCoVars body_ty of
                   ([], _) -> subst
                   (bndrs, _) -> fst $ substTyVarBndrs subst bndrs
    in ConcreteFRR $ substFRROrigin subst' frr_orig

substFRROrigin :: Subst -> FixedRuntimeRepOrigin -> FixedRuntimeRepOrigin
substFRROrigin subst orig@(FixedRuntimeRepOrigin { frr_type = ty })
  = orig { frr_type = substTy subst ty }

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

NB: this is all rather similar to, but sadly not the same as
    Note [Error on unconstrained meta-variables]

Wrinkle:

We must make absolutely sure that alpha indeed is not from an outer
context. (Otherwise, we might indeed learn more information about it.)
This can be done easily: we just check alpha's TcLevel.  That level
must be strictly greater than the ambient TcLevel in order to treat it
as naughty. We say "strictly greater than" because the call to
candidateQTyVars is made outside the bumped TcLevel, as stated in the
comment to candidateQTyVarsOfType. The level check is done in go_tv in
collect_cand_qtvs. Skipping this check caused #16517.

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

isEmptyCandidates :: CandidatesQTvs -> Bool
isEmptyCandidates (DV { dv_kvs = kvs, dv_tvs = tvs })
  = isEmptyDVarSet kvs && isEmptyDVarSet tvs

-- | Extract out the kind vars (in order) and type vars (in order) from
-- a 'CandidatesQTvs'. The lists are guaranteed to be distinct. Keeping
-- the lists separate is important only in the -XNoPolyKinds case.
candidateVars :: CandidatesQTvs -> ([TcTyVar], [TcTyVar])
candidateVars (DV { dv_kvs = dep_kv_set, dv_tvs = nondep_tkv_set })
  = (dep_kvs, nondep_tvs)
  where
    dep_kvs = scopedSort $ dVarSetElems dep_kv_set
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

candidateKindVars :: CandidatesQTvs -> TyVarSet
candidateKindVars dvs = dVarSetToVarSet (dv_kvs dvs)

delCandidates :: CandidatesQTvs -> [Var] -> CandidatesQTvs
delCandidates (DV { dv_kvs = kvs, dv_tvs = tvs, dv_cvs = cvs }) vars
  = DV { dv_kvs = kvs `delDVarSetList` vars
       , dv_tvs = tvs `delDVarSetList` vars
       , dv_cvs = cvs `delVarSetList`  vars }

partitionCandidates :: CandidatesQTvs -> (TyVar -> Bool) -> (TyVarSet, CandidatesQTvs)
-- The selected TyVars are returned as a non-deterministic TyVarSet
partitionCandidates dvs@(DV { dv_kvs = kvs, dv_tvs = tvs }) pred
  = (extracted, dvs { dv_kvs = rest_kvs, dv_tvs = rest_tvs })
  where
    (extracted_kvs, rest_kvs) = partitionDVarSet pred kvs
    (extracted_tvs, rest_tvs) = partitionDVarSet pred tvs
    extracted = dVarSetToVarSet extracted_kvs `unionVarSet` dVarSetToVarSet extracted_tvs

candidateQTyVarsWithBinders :: [TyVar] -> Type -> TcM CandidatesQTvs
-- (candidateQTyVarsWithBinders tvs ty) returns the candidateQTyVars
-- of (forall tvs. ty), but do not treat 'tvs' as bound for the purpose
-- of Note [Naughty quantification candidates].  Why?
-- Because we are going to scoped-sort the quantified variables
-- in among the tvs
candidateQTyVarsWithBinders bound_tvs ty
  = do { kvs     <- candidateQTyVarsOfKinds (map tyVarKind bound_tvs)
       ; cur_lvl <- getTcLevel
       ; all_tvs <- collect_cand_qtvs ty False cur_lvl emptyVarSet kvs ty
       ; return (all_tvs `delCandidates` bound_tvs) }

-- | Gathers free variables to use as quantification candidates (in
-- 'quantifyTyVars'). This might output the same var
-- in both sets, if it's used in both a type and a kind.
-- The variables to quantify must have a TcLevel strictly greater than
-- the ambient level. (See Wrinkle in Note [Naughty quantification candidates])
-- See Note [CandidatesQTvs determinism and order]
-- See Note [Dependent type variables]
candidateQTyVarsOfType :: TcType       -- not necessarily zonked
                       -> TcM CandidatesQTvs
candidateQTyVarsOfType ty
  = do { cur_lvl <- getTcLevel
       ; collect_cand_qtvs ty False cur_lvl emptyVarSet mempty ty }

-- | Like 'candidateQTyVarsOfType', but over a list of types
-- The variables to quantify must have a TcLevel strictly greater than
-- the ambient level. (See Wrinkle in Note [Naughty quantification candidates])
candidateQTyVarsOfTypes :: [Type] -> TcM CandidatesQTvs
candidateQTyVarsOfTypes tys
  = do { cur_lvl <- getTcLevel
       ; foldlM (\acc ty -> collect_cand_qtvs ty False cur_lvl emptyVarSet acc ty)
                mempty tys }

-- | Like 'candidateQTyVarsOfType', but consider every free variable
-- to be dependent. This is appropriate when generalizing a *kind*,
-- instead of a type. (That way, -XNoPolyKinds will default the variables
-- to Type.)
candidateQTyVarsOfKind :: TcKind       -- Not necessarily zonked
                       -> TcM CandidatesQTvs
candidateQTyVarsOfKind ty
  = do { cur_lvl <- getTcLevel
       ; collect_cand_qtvs ty True cur_lvl emptyVarSet mempty ty }

candidateQTyVarsOfKinds :: [TcKind]    -- Not necessarily zonked
                       -> TcM CandidatesQTvs
candidateQTyVarsOfKinds tys
  = do { cur_lvl <- getTcLevel
       ; foldM (\acc ty -> collect_cand_qtvs ty True cur_lvl emptyVarSet acc ty)
               mempty tys }

collect_cand_qtvs
  :: TcType          -- Original type that we started recurring into; for errors
  -> Bool            -- True <=> consider every fv in Type to be dependent
  -> TcLevel         -- Current TcLevel; collect only tyvars whose level is greater
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

collect_cand_qtvs orig_ty is_dep cur_lvl bound dvs ty
  = go dvs ty
  where
    is_bound tv = tv `elemVarSet` bound

    -----------------
    go :: CandidatesQTvs -> TcType -> TcM CandidatesQTvs
    -- Uses accumulating-parameter style
    go dv (AppTy t1 t2)       = foldlM go dv [t1, t2]
    go dv (TyConApp tc tys)   = go_tc_args dv (tyConBinders tc) tys
    go dv (FunTy _ w arg res) = foldlM go dv [w, arg, res]
    go dv (LitTy {})          = return dv
    go dv (CastTy ty co)      = do { dv1 <- go dv ty
                                   ; collect_cand_qtvs_co orig_ty cur_lvl bound dv1 co }
    go dv (CoercionTy co)     = collect_cand_qtvs_co orig_ty cur_lvl bound dv co

    go dv (TyVarTy tv)
      | is_bound tv = return dv
      | otherwise   = do { m_contents <- isFilledMetaTyVar_maybe tv
                         ; case m_contents of
                             Just ind_ty -> go dv ind_ty
                             Nothing     -> go_tv dv tv }

    go dv (ForAllTy (Bndr tv _) ty)
      = do { dv1 <- collect_cand_qtvs orig_ty True cur_lvl bound dv (tyVarKind tv)
           ; collect_cand_qtvs orig_ty is_dep cur_lvl (bound `extendVarSet` tv) dv1 ty }

      -- This makes sure that we default e.g. the alpha in Proxy alpha (Any alpha).
      -- Tested in polykinds/NestedProxies.
      -- We just might get this wrong in AppTy, but I don't think that's possible
      -- with -XNoPolyKinds. And fixing it would be non-performant, as we'd need
      -- to look at kinds.
    go_tc_args dv (tc_bndr:tc_bndrs) (ty:tys)
      = do { dv1 <- collect_cand_qtvs orig_ty (is_dep || isNamedTyConBinder tc_bndr)
                                      cur_lvl bound dv ty
           ; go_tc_args dv1 tc_bndrs tys }
    go_tc_args dv _bndrs tys  -- _bndrs might be non-empty: undersaturation
                              -- tys might be non-empty: oversaturation
                              -- either way, the foldlM is correct
      = foldlM go dv tys

    -----------------
    go_tv dv@(DV { dv_kvs = kvs, dv_tvs = tvs }) tv
      | cur_lvl `deeperThanOrSame` tcTyVarLevel tv
      = return dv   -- This variable is from an outer context; skip
                    -- See Note [Use level numbers for quantification]

      | case tcTyVarDetails tv of
          SkolemTv _ lvl _ -> lvl `strictlyDeeperThan` pushTcLevel cur_lvl
          _                -> False
      = return dv  -- Skip inner skolems
        -- This only happens for erroneous program with bad telescopes
        -- e.g. BadTelescope2:  forall a k (b :: k). SameKind a b
        --      We have (a::k), and at the outer we don't want to quantify
        --      over the already-quantified skolem k.
        -- (Apparently we /do/ want to quantify over skolems whose level sk_lvl is
        -- sk_lvl > cur_lvl; you get lots of failures otherwise. A battle for another day.)

      | tv `elemDVarSet` kvs
      = return dv  -- We have met this tyvar already

      | not is_dep
      , tv `elemDVarSet` tvs
      = return dv  -- We have met this tyvar already

      | otherwise
      = do { tv_kind <- liftZonkM $ zonkTcType (tyVarKind tv)
                 -- This zonk is annoying, but it is necessary, both to
                 -- ensure that the collected candidates have zonked kinds
                 -- (#15795) and to make the naughty check
                 -- (which comes next) works correctly

           ; let tv_kind_vars = tyCoVarsOfType tv_kind
           ; if | intersectsVarSet bound tv_kind_vars
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
                      ; collect_cand_qtvs orig_ty True cur_lvl bound dv' tv_kind } }

collect_cand_qtvs_co :: TcType -- original type at top of recursion; for errors
                     -> TcLevel
                     -> VarSet -- bound variables
                     -> CandidatesQTvs -> Coercion
                     -> TcM CandidatesQTvs
collect_cand_qtvs_co orig_ty cur_lvl bound = go_co
  where
    go_co dv (Refl ty)               = collect_cand_qtvs orig_ty True cur_lvl bound dv ty
    go_co dv (GRefl _ ty mco)        = do { dv1 <- collect_cand_qtvs orig_ty True cur_lvl bound dv ty
                                          ; go_mco dv1 mco }
    go_co dv (TyConAppCo _ _ cos)    = foldlM go_co dv cos
    go_co dv (AppCo co1 co2)         = foldlM go_co dv [co1, co2]
    go_co dv (FunCo _ _ _ w co1 co2) = foldlM go_co dv [w, co1, co2]
    go_co dv (AxiomCo _ cos)         = foldlM go_co dv cos
    go_co dv (UnivCo { uco_lty = t1, uco_rty = t2, uco_deps = deps })
                                     = do { dv1 <- collect_cand_qtvs orig_ty True cur_lvl bound dv t1
                                          ; dv2 <- collect_cand_qtvs orig_ty True cur_lvl bound dv1 t2
                                          ; foldM go_co dv2 deps }
    go_co dv (SymCo co)              = go_co dv co
    go_co dv (TransCo co1 co2)       = foldlM go_co dv [co1, co2]
    go_co dv (SelCo _ co)            = go_co dv co
    go_co dv (LRCo _ co)             = go_co dv co
    go_co dv (InstCo co1 co2)        = foldlM go_co dv [co1, co2]
    go_co dv (KindCo co)             = go_co dv co
    go_co dv (SubCo co)              = go_co dv co

    go_co dv (HoleCo hole)
      = do m_co <- unpackCoercionHole_maybe hole
           case m_co of
             Just co -> go_co dv co
             Nothing -> go_cv dv (coHoleCoVar hole)

    go_co dv (CoVarCo cv) = go_cv dv cv

    go_co dv (ForAllCo { fco_tcv = tcv, fco_kind = kind_co, fco_body = co })
      = do { dv1 <- go_co dv kind_co
           ; collect_cand_qtvs_co orig_ty cur_lvl (bound `extendVarSet` tcv) dv1 co }

    go_mco dv MRefl    = return dv
    go_mco dv (MCo co) = go_co dv co

    go_cv :: CandidatesQTvs -> CoVar -> TcM CandidatesQTvs
    go_cv dv@(DV { dv_cvs = cvs }) cv
      | is_bound cv         = return dv
      | cv `elemVarSet` cvs = return dv

        -- See Note [Recurring into kinds for candidateQTyVars]
      | otherwise           = collect_cand_qtvs orig_ty True cur_lvl bound
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
than the ambient level (see Note [Use level numbers for quantification]).

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
quirks. See, for example, the code in GHC.Tc.Solver.decidePromotedTyVars
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

quantifyTyVars :: SkolemInfo
               -> NonStandardDefaultingStrategy
               -> CandidatesQTvs   -- See Note [Dependent type variables]
                                   -- Already zonked
               -> TcM [TcTyVar]
-- See Note [quantifyTyVars]
-- Can be given a mixture of TcTyVars and TyVars, in the case of
--   associated type declarations. Also accepts covars, but *never* returns any.
-- According to Note [Use level numbers for quantification] and the
-- invariants on CandidateQTvs, we do not have to filter out variables
-- free in the environment here. Just quantify unconditionally, subject
-- to the restrictions in Note [quantifyTyVars].
quantifyTyVars skol_info ns_strat dvs
       -- short-circuit common case
  | isEmptyCandidates dvs
  = do { traceTc "quantifyTyVars has nothing to quantify" empty
       ; return [] }

  | otherwise
  = do { traceTc "quantifyTyVars {"
           ( vcat [ text "ns_strat =" <+> ppr ns_strat
                  , text "dvs =" <+> ppr dvs ])

       ; undefaulted <- defaultTyVars ns_strat dvs
       ; final_qtvs  <- liftZonkM $ mapMaybeM zonk_quant undefaulted

       ; traceTc "quantifyTyVars }"
           (vcat [ text "undefaulted:" <+> pprTyVars undefaulted
                 , text "final_qtvs:"  <+> pprTyVars final_qtvs ])

       -- We should never quantify over coercion variables; check this
       ; let co_vars = filter isCoVar final_qtvs
       ; massertPpr (null co_vars) (ppr co_vars)

       ; return final_qtvs }
  where
    -- zonk_quant returns a tyvar if it should be quantified over;
    -- otherwise, it returns Nothing. The latter case happens for
    -- non-meta-tyvars
    zonk_quant tkv
      | not (isTyVar tkv)
      = return Nothing   -- this can happen for a covar that's associated with
                         -- a coercion hole. Test case: typecheck/should_compile/T2494

      | otherwise
      = Just <$> skolemiseQuantifiedTyVar skol_info tkv

isQuantifiableTv :: TcLevel   -- Level of the context, outside the quantification
                 -> TcTyVar
                 -> Bool
isQuantifiableTv outer_tclvl tcv
  | isTcTyVar tcv  -- Might be a CoVar; change this when gather covars separately
  = tcTyVarLevel tcv `strictlyDeeperThan` outer_tclvl
  | otherwise
  = False

zonkAndSkolemise :: SkolemInfo -> TcTyCoVar -> ZonkM TcTyCoVar
-- A tyvar binder is never a unification variable (TauTv),
-- rather it is always a skolem. It *might* be a TyVarTv.
-- (Because non-CUSK type declarations use TyVarTvs.)
-- Regardless, it may have a kind that has not yet been zonked,
-- and may include kind unification variables.
zonkAndSkolemise skol_info tyvar
  | isTyVarTyVar tyvar
     -- We want to preserve the binding location of the original TyVarTv.
     -- This is important for error messages. If we don't do this, then
     -- we get bad locations in, e.g., typecheck/should_fail/T2688
  = do { zonked_tyvar <- zonkTcTyVarToTcTyVar tyvar
       ; skolemiseQuantifiedTyVar skol_info zonked_tyvar }

  | otherwise
  = assertPpr (isImmutableTyVar tyvar || isCoVar tyvar) (pprTyVar tyvar) $
    zonkTyCoVarKind tyvar

skolemiseQuantifiedTyVar :: SkolemInfo -> TcTyVar -> ZonkM TcTyVar
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

skolemiseQuantifiedTyVar skol_info tv
  = case tcTyVarDetails tv of
      MetaTv {} -> skolemiseUnboundMetaTyVar skol_info tv

      SkolemTv _ lvl _  -- It might be a skolem type variable,
                        -- for example from a user type signature
        -- But it might also be a shared meta-variable across several
        -- type declarations, each with its own skol_info. The first
        -- will skolemise it, but the other uses must update its
        -- skolem info (#22379)
        -> do { kind <- zonkTcType (tyVarKind tv)
              ; let details = SkolemTv skol_info lvl False
                    name = tyVarName tv
              ; return (mkTcTyVar name kind details) }

      _other -> pprPanic "skolemiseQuantifiedTyVar" (ppr tv) -- RuntimeUnk

-- | Default a type variable using the given defaulting strategy.
--
-- See Note [Type variable defaulting options] in GHC.Types.Basic.
defaultTyVar :: DefaultingStrategy
             -> TcTyVar    -- If it's a MetaTyVar then it is unbound
             -> TcM Bool   -- True <=> defaulted away altogether
defaultTyVar def_strat tv
  | not (isMetaTyVar tv)
  || isTyVarTyVar tv
    -- Do not default TyVarTvs. Doing so would violate the invariants
    -- on TyVarTvs; see Note [TyVarTv] in GHC.Tc.Utils.TcMType.
    -- #13343 is an example; #14555 is another
    -- See Note [Inferring kinds for type declarations] in GHC.Tc.TyCl
  = return False

  | isRuntimeRepVar tv
  , default_ns_vars
  = do { traceTc "Defaulting a RuntimeRep var to LiftedRep" (ppr tv)
       ; liftZonkM $ writeMetaTyVar tv liftedRepTy
       ; return True }

  | isLevityVar tv
  , default_ns_vars
  = do { traceTc "Defaulting a Levity var to Lifted" (ppr tv)
       ; liftZonkM $ writeMetaTyVar tv liftedDataConTy
       ; return True }

  | isMultiplicityVar tv
  , default_ns_vars
  = do { traceTc "Defaulting a Multiplicity var to Many" (ppr tv)
       ; liftZonkM $ writeMetaTyVar tv manyDataConTy
       ; return True }

  | isConcreteTyVar tv
    -- We don't want to quantify; but neither can we default to
    -- anything sensible.  (If it has kind RuntimeRep or Levity, as is
    -- often the case, it'll have been caught earlier by earlier
    -- cases. So in this exotic situation we just promote.  Not very
    -- satisfing, but it's very much a corner case: #23051
    -- We should really implement the plan in #20686.
  = do { lvl <- getTcLevel
       ; _ <- promoteMetaTyVarTo lvl tv
       ; return True }

  | DefaultKindVars <- def_strat -- -XNoPolyKinds and this is a kind var: we must default it
  = default_kind_var tv

  | otherwise
  = return False

  where
    default_ns_vars :: Bool
    default_ns_vars = defaultNonStandardTyVars def_strat
    default_kind_var :: TyVar -> TcM Bool
       -- defaultKindVar is used exclusively with -XNoPolyKinds
       -- See Note [Defaulting with -XNoPolyKinds]
       -- It takes an (unconstrained) meta tyvar and defaults it.
       -- Works only on vars of type *; for other kinds, it issues an error.
    default_kind_var kv
      | isLiftedTypeKind (tyVarKind kv)
      = do { traceTc "Defaulting a kind var to *" (ppr kv)
           ; liftZonkM $ writeMetaTyVar kv liftedTypeKind
           ; return True }
      | otherwise
      = do { let (tidy_env, kv') = tidyFreeTyCoVarX emptyTidyEnv kv
           ; addErrTcM $ (tidy_env, TcRnCannotDefaultKindVar kv' (tyVarKind kv'))
           -- We failed to default it, so return False to say so.
           -- Hence, it'll get skolemised.  That might seem odd, but we must either
           -- promote, skolemise, or zap-to-Any, to satisfy GHC.Tc.Gen.HsType
           --    Note [Recipe for checking a signature]
           -- Otherwise we get level-number assertion failures. It doesn't matter much
           -- because we are in an error situation anyway.
           ; return False
        }

-- | Default some unconstrained type variables, as specified
-- by the defaulting options:
--
--  - 'RuntimeRep' tyvars default to 'LiftedRep'
--  - 'Levity' tyvars default to 'Lifted'
--  - 'Multiplicity' tyvars default to 'Many'
--  - 'Type' tyvars from dv_kvs default to 'Type', when -XNoPolyKinds
--    (under -XNoPolyKinds, non-defaulting vars in dv_kvs is an error)
defaultTyVars :: NonStandardDefaultingStrategy
              -> CandidatesQTvs    -- ^ all candidates for quantification
              -> TcM [TcTyVar]     -- ^ those variables not defaulted
defaultTyVars ns_strat dvs
  = do { poly_kinds <- xoptM LangExt.PolyKinds
       ; let
           def_tvs, def_kvs :: DefaultingStrategy
           def_tvs = NonStandardDefaulting ns_strat
           def_kvs | poly_kinds = def_tvs
                   | otherwise  = DefaultKindVars
             -- As -XNoPolyKinds precludes polymorphic kind variables, we default them.
             -- For example:
             --
             --   type F :: Type -> Type
             --   type family F a where
             --      F (a -> b) = b
             --
             -- Here we get `a :: TYPE r`, so to accept this program when -XNoPolyKinds is enabled
             -- we must default the kind variable `r :: RuntimeRep`.
             -- Test case: T20584.
       ; defaulted_kvs <- mapM (defaultTyVar def_kvs) dep_kvs
       ; defaulted_tvs <- mapM (defaultTyVar def_tvs) nondep_tvs
       ; let undefaulted_kvs = [ kv | (kv, False) <- dep_kvs    `zip` defaulted_kvs ]
             undefaulted_tvs = [ tv | (tv, False) <- nondep_tvs `zip` defaulted_tvs ]
       ; return (undefaulted_kvs ++ undefaulted_tvs) }
          -- NB: kvs before tvs because tvs may depend on kvs
  where
    (dep_kvs, nondep_tvs) = candidateVars dvs

skolemiseUnboundMetaTyVar :: SkolemInfo -> TcTyVar -> ZonkM TyVar
-- We have a Meta tyvar with a ref-cell inside it
-- Skolemise it, so that we are totally out of Meta-tyvar-land
-- We create a skolem TcTyVar, not a regular TyVar
--   See Note [Zonking to Skolem]
--
-- Its level should be one greater than the ambient level, which will typically
-- be the same as the level on the meta-tyvar. But not invariably; for example
--    f :: (forall a b. SameKind a b) -> Int
-- The skolems 'a' and 'b' are bound by tcTKTelescope, at level 2; and they each
-- have a level-2 kind unification variable, since it might get unified with another
-- of the level-2 skolems e.g. 'k' in this version
--    f :: (forall k (a :: k) b. SameKind a b) -> Int
-- So when we quantify the kind vars at the top level of the signature, the ambient
-- level is 1, but we will quantify over kappa[2].

skolemiseUnboundMetaTyVar skol_info tv
  = assertPpr (isMetaTyVar tv) (ppr tv) $
    do  { check_empty tv
        -- Get the location and level from "here",
        -- i.e. where we are generalising
        ; ZonkGblEnv { zge_src_span = here, zge_tc_level = tc_lvl }
           <- getZonkGblEnv
        ; kind   <- zonkTcType (tyVarKind tv)
        ; let tv_name = tyVarName tv
              -- See Note [Skolemising and identity]
              final_name | isSystemName tv_name
                         = mkInternalName (nameUnique tv_name)
                                          (nameOccName tv_name) here
                         | otherwise
                         = tv_name
              details    = SkolemTv skol_info (pushTcLevel tc_lvl) False
              final_tv   = mkTcTyVar final_name kind details

        ; traceZonk "Skolemising" (ppr tv <+> text ":=" <+> ppr final_tv)
        ; writeMetaTyVar tv (mkTyVarTy final_tv)
        ; return final_tv }
  where
    check_empty tv       -- [Sept 04] Check for non-empty.
      = when debugIsOn $  -- See Note [Silly Type Synonyms]
        do { cts <- readMetaTyVar tv
           ; case cts of
               Flexi       -> return ()
               Indirect ty -> warnPprTrace True "skolemiseUnboundMetaTyVar" (ppr tv $$ ppr ty) $
                              return () }

{- Note [Error on unconstrained meta-variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

* type C :: Type -> Type -> Constraint
  class (forall a. a b ~ a c) => C b c

* type T = forall a. Proxy a

* data (forall a. a b ~ a c) => T b c

* type instance F Int = Proxy Any
  where Any :: forall k. k

In the first three cases we will infer a :: Type -> kappa, but then
we get no further information on kappa. In the last, we will get
  Proxy kappa Any
but again will get no further info on kappa.

What do do?
 A. We could choose kappa := Type. But this only works when the kind of kappa
    is Type (true in this example, but not always).
 B. We could default to Any.
 C. We could quantify.
 D. We could error.

We choose (D), as described in #17567, and implement this choice in
doNotQuantifyTyVars.  Discussion of alternatives A-C is below.

NB: this is all rather similar to, but sadly not the same as
    Note [Naughty quantification candidates]

To do this, we must take an extra step before doing the final zonk to create
e.g. a TyCon. (There is no problem in the final term-level zonk. See the
section on alternative (B) below.) This extra step is needed only for
constructs that do not quantify their free meta-variables, such as a class
constraint or right-hand side of a type synonym.

Specifically: before the final zonk, every construct must either call
quantifyTyVars or doNotQuantifyTyVars. The latter issues an error
if it is passed any free variables. (Exception: we still default
RuntimeRep and Multiplicity variables.)

Because no meta-variables remain after quantifying or erroring, we perform
the zonk with NoFlexi, which panics upon seeing a meta-variable.

Alternatives A-C, not implemented:

A. As stated above, this works only sometimes. We might have a free
   meta-variable of kind Nat, for example.

B. This is what we used to do, but it caused Any to appear in error
   messages sometimes. See #17567 for several examples. Defaulting to
   Any during the final, whole-program zonk is OK, though, because
   we are completely done type-checking at that point. No chance to
   leak into an error message.

C. Examine the class declaration at the top of this Note again.
   Where should we quantify? We might imagine quantifying and
   putting the kind variable in the forall of the quantified constraint.
   But what if there are nested foralls? Which one should get the
   variable? Other constructs have other problems. (For example,
   the right-hand side of a type family instance equation may not
   be a poly-type.)

   More broadly, the GHC AST defines a set of places where it performs
   implicit lexical generalization. For example, in a type
   signature

     f :: Proxy a -> Bool

   the otherwise-unbound a is lexically quantified, giving us

     f :: forall a. Proxy a -> Bool

   The places that allow lexical quantification are marked in the AST with
   HsImplicitBndrs. HsImplicitBndrs offers a binding site for otherwise-unbound
   variables.

   Later, during type-checking, we discover that a's kind is unconstrained.
   We thus quantify *again*, to

     f :: forall {k} (a :: k). Proxy @k a -> Bool

   It is this second quantification that this Note is really about --
   let's call it *inferred quantification*.
   So there are two sorts of implicit quantification in types:
     1. Lexical quantification: signalled by HsImplicitBndrs, occurs over
        variables mentioned by the user but with no explicit binding site,
        suppressed by a user-written forall (by the forall-or-nothing rule,
        in Note [forall-or-nothing rule] in GHC.Hs.Type).
     2. Inferred quantification: no signal in HsSyn, occurs over unconstrained
        variables invented by the type-checker, possible only with -XPolyKinds,
        unaffected by forall-or-nothing rule
   These two quantifications are performed in different compiler phases, and are
   essentially unrelated. However, it is convenient
   for programmers to remember only one set of implicit quantification
   sites. So, we choose to use the same places (those with HsImplicitBndrs)
   for lexical quantification as for inferred quantification of unconstrained
   meta-variables. Accordingly, there is no quantification in a class
   constraint, or the other constructs that call doNotQuantifyTyVars.
-}

doNotQuantifyTyVars :: CandidatesQTvs
                    -> (TidyEnv -> ZonkM (TidyEnv, UninferrableTyVarCtx))
                            -- ^ like "the class context (D a b, E foogle)"
                    -> TcM ()
-- See Note [Error on unconstrained meta-variables]
doNotQuantifyTyVars dvs where_found
  | isEmptyCandidates dvs
  = traceTc "doNotQuantifyTyVars has nothing to error on" empty

  | otherwise
  = do { traceTc "doNotQuantifyTyVars" (ppr dvs)
       ; undefaulted <- defaultTyVars DefaultNonStandardTyVars dvs
          -- could have regular TyVars here, in an associated type RHS, or
          -- bound by a type declaration head. So filter looking only for
          -- metavars. e.g. b and c in `class (forall a. a b ~ a c) => C b c`
          -- are OK
       ; let leftover_metas = filter isMetaTyVar undefaulted
       ; unless (null leftover_metas) $
         do { let (tidy_env1, tidied_tvs) = tidyFreeTyCoVarsX emptyTidyEnv leftover_metas
            ; (tidy_env2, where_doc) <- liftZonkM $ where_found tidy_env1
            ; let msg = TcRnUninferrableTyVar tidied_tvs where_doc
            ; failWithTcM (tidy_env2, msg) }
       ; traceTc "doNotQuantifyTyVars success" empty }

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
  Note (Sept 04): now that we are zonking quantified type variables
  on construction, the 'a' will be frozen as a regular tyvar on
  quantification, so the floated dict will still have type (C d a).
  Which renders this whole note moot; happily!]

* Then the \/\a abstraction has a zonked 'a' in it.

All very silly.   I think its harmless to ignore the problem.  We'll end up with
a \/\a in the final result but all the occurrences of a will be zonked to ()
-}

--------------------------------------------------------------------------------

-- | @tcCheckUsage name mult thing_inside@ runs @thing_inside@, checks that the
-- usage of @name@ is a submultiplicity of @mult@, and removes @name@ from the
-- usage environment. See also Note [Coercions returned from tcSubMult] in
-- GHC.Tc.Utils.Unify, which applies to the wrapper returned from this function.
tcCheckUsage :: Name -> Mult -> TcM a -> TcM (a, HsWrapper)
tcCheckUsage name id_mult thing_inside
  = do { (local_usage, result) <- tcCollectingUsage thing_inside
       ; wrapper <- check_then_add_usage local_usage
       ; return (result, wrapper) }
    where
    check_then_add_usage :: UsageEnv -> TcM HsWrapper
    -- Checks that the usage of the newly introduced binder is compatible with
    -- its multiplicity, and combines the usage of non-new binders to |uenv|
    check_then_add_usage uenv
      = do { let actual_u = lookupUE uenv name
           ; traceTc "check_then_add_usage" (ppr id_mult $$ ppr actual_u)
           ; wrapper <- case actual_u of
               Bottom -> return idHsWrapper
               Zero     -> tcSubMult (UsageEnvironmentOf name) ManyTy id_mult
               MUsage m -> do { m <- promote_mult m
                              ; tcSubMult (UsageEnvironmentOf name) m id_mult }
           ; tcEmitBindingUsage (deleteUE uenv name)
           ; return wrapper }

    -- This is gross. The problem is in test case typecheck/should_compile/T18998:
    --   f :: a %1-> Id n a -> Id n a
    --   f x (MkId _) = MkId x
    -- where MkId is a GADT constructor. Multiplicity polymorphism of constructors
    -- invents a new multiplicity variable p[2] for the application MkId x. This
    -- variable is at level 2, bumped because of the GADT pattern-match (MkId _).
    -- We eventually unify the variable with One, due to the call to tcSubMult in
    -- tcCheckUsage. But by then, we're at TcLevel 1, and so the level-check
    -- fails.
    --
    -- What to do? If we did inference "for real", the sub-multiplicity constraint
    -- would end up in the implication of the GADT pattern-match, and all would
    -- be well. But we don't have a real sub-multiplicity constraint to put in
    -- the implication. (Multiplicity inference works outside the usual generate-
    -- constraints-and-solve scheme.) Here, where the multiplicity arrives, we
    -- must promote all multiplicity variables to reflect this outer TcLevel.
    -- It's reminiscent of floating a constraint, really, so promotion is
    -- appropriate. The promoteTcType function works only on types of kind TYPE rr,
    -- so we can't use it here. Thus, this dirtiness.
    --
    -- It works nicely in practice.
    --
    -- We use a set to avoid calling promoteMetaTyVarTo twice on the same
    -- metavariable. This happened in #19400.
    promote_mult m = do { fvs <- liftZonkM $ zonkTyCoVarsAndFV (tyCoVarsOfType m)
                        ; any_promoted <- promoteTyVarSet fvs
                        ; if any_promoted then liftZonkM $ zonkTcType m else return m
                        }

{- *********************************************************************
*                                                                      *
         Short-cuts for overloaded numeric literals
*                                                                      *
********************************************************************* -}

-- Overloaded literals. Here mainly because it uses isIntTy etc

{- Note [Short cut for overloaded literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A literal like "3" means (fromInteger @ty (dNum :: Num ty) (3::Integer)).
But if we have a list like
  [4,2,3,2,4,4,2]::[Int]
we use a lot of compile time and space generating and solving all those Num
constraints, and generating calls to fromInteger etc.  Better just to cut to
the chase, and cough up an Int literal. Large collections of literals like this
sometimes appear in source files, so it's quite a worthwhile fix.

So we try to take advantage of whatever nearby type information we have,
to short-cut the process for built-in types.  We can do this in two places;

* In the typechecker, when we are about to typecheck the literal.
* If that fails, in the desugarer, once we know the final type.
-}

tcShortCutLit :: HsOverLit GhcRn -> ExpRhoType -> TcM (Maybe (HsOverLit GhcTc))
tcShortCutLit lit@(OverLit { ol_val = val, ol_ext = OverLitRn rebindable _}) exp_res_ty
  | not rebindable
  , Just res_ty <- checkingExpType_maybe exp_res_ty
  = do { dflags <- getDynFlags
       ; let platform = targetPlatform dflags
       ; case shortCutLit platform val res_ty of
            Just expr -> return $ Just $
                         lit { ol_ext = OverLitTc False expr res_ty }
            Nothing   -> return Nothing }
  | otherwise
  = return Nothing

shortCutLit :: Platform -> OverLitVal -> TcType -> Maybe (HsExpr GhcTc)
shortCutLit platform val res_ty
  = case val of
      HsIntegral int_lit    -> go_integral int_lit
      HsFractional frac_lit -> go_fractional frac_lit
      HsIsString s_lit      -> go_string (sl_st s_lit) (sl_fs s_lit)
  where
    go_integral int@(IL src neg i)
      | isIntTy res_ty  && platformInIntRange  platform i
      = Just (HsLit noExtField (HsInt noExtField int))
      | isWordTy res_ty && platformInWordRange platform i
      = Just (mkLit wordDataCon (HsWordPrim src i))
      | isIntegerTy res_ty
      = Just (HsLit noExtField (HsInteger src i res_ty))
      | otherwise
      = go_fractional (integralFractionalLit neg i)
        -- The 'otherwise' case is important
        -- Consider (3 :: Float).  Syntactically it looks like an IntLit,
        -- so we'll call shortCutIntLit, but of course it's a float
        -- This can make a big difference for programs with a lot of
        -- literals, compiled without -O

    go_fractional f
      | isFloatTy res_ty && valueInRange  = Just (mkLit floatDataCon  (HsFloatPrim noExtField f))
      | isDoubleTy res_ty && valueInRange = Just (mkLit doubleDataCon (HsDoublePrim noExtField f))
      | otherwise                         = Nothing
      where
        valueInRange =
          case f of
            FL { fl_exp = e } -> (-100) <= e && e <= 100
            -- We limit short-cutting Fractional Literals to when their power of 10
            -- is less than 100, which ensures desugaring isn't slow.

    go_string src s
      | isStringTy res_ty = Just (HsLit noExtField (HsString src s))
      | otherwise         = Nothing

mkLit :: DataCon -> HsLit GhcTc -> HsExpr GhcTc
mkLit con lit = HsApp noExtField (nlHsDataCon con) (nlHsLit lit)

------------------------------
hsOverLitName :: OverLitVal -> Name
-- Get the canonical 'fromX' name for a particular OverLitVal
hsOverLitName (HsIntegral {})   = fromIntegerName
hsOverLitName (HsFractional {}) = fromRationalName
hsOverLitName (HsIsString {})   = fromStringName


{- *********************************************************************
*                                                                      *
              Promotion
*                                                                      *
********************************************************************* -}

promoteMetaTyVarTo :: HasDebugCallStack => TcLevel -> TcTyVar -> TcM Bool
-- When we float a constraint out of an implication we must restore
-- invariant (WantedInv) in Note [TcLevel invariants] in GHC.Tc.Utils.TcType
-- Return True <=> we did some promotion
-- Also returns either the original tyvar (no promotion) or the new one
-- See Note [Promoting unification variables]
promoteMetaTyVarTo tclvl tv
  | assertPpr (isMetaTyVar tv) (ppr tv) $
    tcTyVarLevel tv `strictlyDeeperThan` tclvl
  = do { cloned_tv <- cloneMetaTyVar tv
       ; let rhs_tv = setMetaTyVarTcLevel cloned_tv tclvl
       ; liftZonkM $ writeMetaTyVar tv (mkTyVarTy rhs_tv)
       ; traceTc "promoteTyVar" (ppr tv <+> text "-->" <+> ppr rhs_tv)
       ; return True }
   | otherwise
   = return False

-- Returns whether or not *any* tyvar is defaulted
promoteTyVarSet :: HasDebugCallStack => TcTyVarSet -> TcM Bool
promoteTyVarSet tvs
  = do { tclvl <- getTcLevel
       ; bools <- mapM (promoteMetaTyVarTo tclvl)  $
                  filter isPromotableMetaTyVar $
                  nonDetEltsUniqSet tvs
         -- Non-determinism is OK because order of promotion doesn't matter
       ; return (or bools) }

{-
%************************************************************************
%*                                                                      *
             Representation polymorphism checks
*                                                                       *
***********************************************************************-}

-- | Check that the specified type has a fixed runtime representation.
--
-- If it isn't, throw a representation-polymorphism error appropriate
-- for the context (as specified by the 'FixedRuntimeRepProvenance').
--
-- Unlike the other representation polymorphism checks, which can emit
-- new Wanted constraints to be solved by the constraint solver, this function
-- does not emit any constraints: it has enough information to immediately
-- make a decision.
--
-- See (1) in Note [Representation polymorphism checking] in GHC.Tc.Utils.Concrete
checkTypeHasFixedRuntimeRep :: FixedRuntimeRepProvenance -> Type -> TcM ()
checkTypeHasFixedRuntimeRep prov ty =
  unless (typeHasFixedRuntimeRep ty)
    (addDetailedDiagnostic $ TcRnTypeDoesNotHaveFixedRuntimeRep ty prov)

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
  = do { (orig_ty1, escapees') <- liftZonkM $
           do { orig_ty1  <- zonkTcType orig_ty  -- in case it's not zonked
              ; escapees' <- zonkTcTyVarsToTcTyVars $
                             nonDetEltsUniqSet escapees
                             -- we'll just be printing, so no harmful non-determinism
              ; return (orig_ty1, escapees') }

       ; let fvs  = tyCoVarsOfTypeList orig_ty1
             env0 = tidyFreeTyCoVars emptyTidyEnv fvs
             env  = env0 `delTidyEnvList` escapees'
                    -- this avoids gratuitous renaming of the escaped
                    -- variables; very confusing to users!

             orig_ty'   = tidyType env orig_ty1
             tidied = map (tidyTyCoVarOcc env) escapees'
             msg = TcRnSkolemEscape tidied (tidyTyCoVarOcc env tv) orig_ty'

       ; failWithTcM (env, msg) }
