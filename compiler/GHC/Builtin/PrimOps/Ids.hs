
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

-- | PrimOp's Ids
module GHC.Builtin.PrimOps.Ids
  ( primOpId
  , allThePrimOpIds
  , allExposedPrimOpIds
  )
where

import GHC.Prelude

-- primop rules are attached to primop ids
import {-# SOURCE #-} GHC.Core.Opt.ConstantFold (primOpRules)
import GHC.Core.TyCo.Rep ( scaledThing )
import GHC.Core.Type
import GHC.Core.Predicate( tyCoVarsOfTypeWellScoped )
import GHC.Core.FVs (mkRuleInfo)

import GHC.Builtin.PrimOps
import GHC.Builtin.Uniques
import GHC.Builtin.Names
import GHC.Builtin.Types.Prim

import GHC.Types.Basic
import GHC.Types.Cpr
import GHC.Types.Demand
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.TyThing
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Var
import GHC.Types.Var.Set

import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType ( ConcreteTvOrigin(..), ConcreteTyVars, TcType )

import GHC.Data.SmallArray

import Data.Maybe ( mapMaybe, listToMaybe, catMaybes, maybeToList )


-- | Build a PrimOp Id
mkPrimOpId :: PrimOp -> Id
mkPrimOpId prim_op
  = id
  where
    (tyvars,arg_tys,res_ty, arity, strict_sig) = primOpSig prim_op
    ty   = mkForAllTys tyvars (mkVisFunTysMany arg_tys res_ty)
    name = mkWiredInName gHC_PRIM (primOpOcc prim_op)
                         (mkPrimOpIdUnique (primOpTag prim_op))
                         (AnId id) UserSyntax
    id   = mkGlobalId (PrimOpId prim_op conc_tvs) name ty info

    conc_tvs = computePrimOpConcTyVarsFromType name tyvars arg_tys res_ty

    -- PrimOps don't ever construct a product, but we want to preserve bottoms
    cpr
      | isDeadEndDiv (snd (splitDmdSig strict_sig)) = botCpr
      | otherwise                                   = topCpr

    info = noCafIdInfo
           `setRuleInfo`           mkRuleInfo (maybeToList $ primOpRules name prim_op)
           `setArityInfo`          arity
           `setDmdSigInfo`         strict_sig
           `setCprSigInfo`         mkCprSig arity cpr
           `setInlinePragInfo`     neverInlinePragma
               -- We give PrimOps a NOINLINE pragma so that we don't
               -- get silly warnings from Desugar.dsRule (the inline_shadows_rule
               -- test) about a RULE conflicting with a possible inlining
               -- cf #7287

-- | Analyse the type of a primop to determine which of its outermost forall'd
-- type variables must be instantiated to concrete types when the primop is
-- instantiated.
--
-- These are the Levity and RuntimeRep kinded type-variables which appear in
-- negative position in the type of the primop.
computePrimOpConcTyVarsFromType :: Name -> [TyVarBinder] -> [Type] -> Type -> ConcreteTyVars
computePrimOpConcTyVarsFromType nm tyvars arg_tys _res_ty = mkNameEnv concs
  where
    concs = [ (tyVarName kind_tv, ConcreteFRR frr_orig)
            | Bndr tv _af <- tyvars
            , kind_tv    <- tyCoVarsOfTypeWellScoped $ tyVarKind tv
            , neg_pos    <- maybeToList $ frr_tyvar_maybe kind_tv
            , let frr_orig = FixedRuntimeRepOrigin
                           { frr_type    = mkTyVarTy tv
                           , frr_context = FRRRepPolyId nm RepPolyPrimOp neg_pos
                           }
            ]

    -- As per Note [Levity and representation polymorphic primops]
    -- in GHC.Builtin.Primops.txt.pp, we compute the ConcreteTyVars associated
    -- to a primop by inspecting the type variable names.
    frr_tyvar_maybe tv
      | tv `elem` [ runtimeRep1TyVar, runtimeRep2TyVar, runtimeRep3TyVar
                  , levity1TyVar, levity2TyVar ]
      = listToMaybe $
          mapMaybe (\ (i,arg) -> Argument i <$> positiveKindPos_maybe tv arg)
            (zip [1..] arg_tys)
      | otherwise
      = Nothing
      -- Compute whether the type variable occurs in the kind of a type variable
      -- in positive position in one of the argument types of the primop.

-- | Does this type variable appear in a kind in a negative position in the
-- type?
--
-- Returns the first such position if so.
--
-- NB: assumes the type is of a simple form, e.g. no foralls, no function
-- arrows nested in a TyCon other than a function arrow.
-- Just used to compute the set of ConcreteTyVars for a PrimOp by inspecting
-- its type, see 'computePrimOpConcTyVarsFromType'.
negativeKindPos_maybe :: TcTyVar -> TcType -> Maybe (Position Neg)
negativeKindPos_maybe tv ty
  | (args, res) <- splitFunTys ty
  = listToMaybe $ catMaybes $
      ( (if null args then Nothing else Result <$> negativeKindPos_maybe tv res)
      : map recur (zip [1..] args)
      )
  where
    recur (pos, scaled_ty)
      = Argument pos <$> positiveKindPos_maybe tv (scaledThing scaled_ty)
    -- (assumes we don't have any function types nested inside other types)

-- | Does this type variable appear in a kind in a positive position in the
-- type?
--
-- Returns the first such position if so.
--
-- NB: assumes the type is of a simple form, e.g. no foralls, no function
-- arrows nested in a TyCon other than a function arrow.
-- Just used to compute the set of ConcreteTyVars for a PrimOp by inspecting
-- its type, see 'computePrimOpConcTyVarsFromType'.
positiveKindPos_maybe :: TcTyVar -> TcType -> Maybe (Position Pos)
positiveKindPos_maybe tv ty
  | (args, res) <- splitFunTys ty
  = listToMaybe $ catMaybes $
      ( (if null args then finish res else Result <$> positiveKindPos_maybe tv res)
      : map recur (zip [1..] args)
      )
  where
    recur (pos, scaled_ty)
      = Argument pos <$> negativeKindPos_maybe tv (scaledThing scaled_ty)
    -- (assumes we don't have any function types nested inside other types)
    finish ty
      | tv `elemVarSet` tyCoVarsOfType (typeKind ty)
      = Just Top
      | otherwise
      = Nothing

-------------------------------------------------------------
-- Cache of PrimOp's Ids
-------------------------------------------------------------

-- | A cache of the PrimOp Ids, indexed by PrimOp tag (0 indexed)
primOpIds :: SmallArray Id
{-# NOINLINE primOpIds #-}
primOpIds = listToArray (maxPrimOpTag+1) primOpTag mkPrimOpId allThePrimOps

-- | Get primop id.
--
-- Retrieve it from `primOpIds` cache.
primOpId :: PrimOp -> Id
{-# INLINE primOpId #-}
primOpId op = indexSmallArray primOpIds (primOpTag op)

-- | All the primop ids, as a list
allThePrimOpIds :: [Id]
{-# INLINE allThePrimOpIds #-}
allThePrimOpIds = map (indexSmallArray primOpIds) [0..maxPrimOpTag]

-- | All the Ids for primops that are exposed from GHC.Prim, as a list.
allExposedPrimOpIds :: [Id]
allExposedPrimOpIds = map primOpId $ filter primOpIsExposed allThePrimOps
