-- | PrimOp's Ids
module GHC.Builtin.PrimOps.Ids
  ( primOpId
  , allThePrimOpIds
  )
where

import GHC.Prelude

-- primop rules are attached to primop ids
import {-# SOURCE #-} GHC.Core.Opt.ConstantFold (primOpRules)
import GHC.Core.Type (mkForAllTys, mkVisFunTysMany)
import GHC.Core.FVs (mkRuleInfo)

import GHC.Builtin.PrimOps
import GHC.Builtin.Uniques
import GHC.Builtin.Names

import GHC.Types.Basic
import GHC.Types.Cpr
import GHC.Types.Demand
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.TyThing
import GHC.Types.Name

import GHC.Data.SmallArray
import Data.Maybe ( maybeToList )


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
    id   = mkGlobalId (PrimOpId prim_op) name ty info

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
           `setLevityInfoWithType` res_ty
               -- We give PrimOps a NOINLINE pragma so that we don't
               -- get silly warnings from Desugar.dsRule (the inline_shadows_rule
               -- test) about a RULE conflicting with a possible inlining
               -- cf #7287


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
