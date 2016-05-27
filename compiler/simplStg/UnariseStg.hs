{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-2012


Note [Unarisation]
~~~~~~~~~~~~~~~~~~
The idea of this pass is to translate away *all* unboxed-tuple binders.
So for example:

f (x :: (# Int, Bool #)) = f x + f (# 1, True #)
 ==>
f (x1 :: Int) (x2 :: Bool) = f x1 x2 + f 1 True

It is important that we do this at the STG level and NOT at the core level
because it would be very hard to make this pass Core-type-preserving.

STG fed to the code generators *must* be unarised because the code generators do
not support unboxed tuple binders natively.

In more detail:

Suppose that a variable x : (# t1, t2 #).

  * At the binding site for x, make up fresh vars  x1:t1, x2:t2

  * Extend the UniariseEnv   x :-> [x1,x2]

  * Replace the binding with a curried binding for x1,x2
       Lambda:   \x.e                ==>   \x1 x2. e
       Case alt: MkT a b x c d -> e  ==>   MkT a b x1 x2 c d -> e

  * Replace argument occurrences with a sequence of args
    via a lookup in UnariseEnv
       f a b x c d   ==>   f a b x1 x2 c d

  * Replace tail-call occurrences with an unboxed tuple
    via a lookup in UnariseEnv
       x  ==>  (# x1, x2 #)
    So, for example
       f x = x    ==>   f x1 x2 = (# x1, x2 #)

    This applies to case scrutinees too
       case x of (# a,b #) -> e   ==>   case (# x1,x2 #) of (# a,b #) -> e
    I think we rely on the code generator to short-circuit this
    case without generating any actual code.

Of course all this applies recursively, so that we flattn out nested tuples.

Note [Unarisation and nullary tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The above scheme has a special cases for nullary unboxed tuples, x :: (# #)

  * Extend the UnariseEnv with   x :-> [voidPrimId]

  * Replace bindings with a binding for void:Void#
       \x. e  =>  \void. e
    and similarly case alternatives

  * If we find (# #) as an argument all by itself
       f ...(# #)...
    it looks like an Id, so we look up in UnariseEnv. We want to replace it
    with voidPrimId, so the convenient thing is to initalise the UniariseEnv
    with   (# #) :-> [voidPrimId]

See also Note [Nullary unboxed tuple] in Type.hs.

Note [Unarisation and arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because of unarisation, the arity that will be recorded in the generated info table
for an Id may be larger than the idArity. Instead we record what we call the RepArity,
which is the Arity taking into account any expanded arguments, and corresponds to
the number of (possibly-void) *registers* arguments will arrive in.
-}

{-# LANGUAGE CPP #-}

module UnariseStg (unarise) where

#include "HsVersions.h"

import CoreSyn
import StgSyn
import VarEnv
import UniqSupply
import Id
import MkId ( voidPrimId, voidArgId )
import Type
import TysWiredIn
import DataCon
import VarSet
import OccName
import Name
import Util
import Outputable
import BasicTypes


-- | A mapping from unboxed-tuple binders to the Ids they were expanded to.
--
-- INVARIANT: Ids in the range don't have unboxed tuple types.
--
-- Those in-scope variables without unboxed-tuple types are not present in
-- the domain of the mapping at all.
type UnariseEnv = VarEnv [Id]

unarise :: UniqSupply -> [StgBinding] -> [StgBinding]
unarise us binds = zipWith (\us -> unariseBinding us init_env) (listSplitUniqSupply us) binds
  where
     -- See Note [Unarisation and nullary tuples]
     nullary_tup = dataConWorkId unboxedUnitDataCon
     init_env = unitVarEnv nullary_tup [voidPrimId]

unariseBinding :: UniqSupply -> UnariseEnv -> StgBinding -> StgBinding
unariseBinding us rho bind = case bind of
  StgNonRec x rhs -> StgNonRec x (unariseRhs us rho rhs)
  StgRec xrhss    -> StgRec $ zipWith (\us (x, rhs) -> (x, unariseRhs us rho rhs))
                                      (listSplitUniqSupply us) xrhss

unariseRhs :: UniqSupply -> UnariseEnv -> StgRhs -> StgRhs
unariseRhs us rho rhs = case rhs of
  StgRhsClosure ccs b_info fvs update_flag srt args expr
    -> StgRhsClosure ccs b_info (unariseIds rho fvs) update_flag
                     (unariseSRT rho srt) args' (unariseExpr us' rho' expr)
    where (us', rho', args') = unariseIdBinders us rho args
  StgRhsCon ccs con args
    -> StgRhsCon ccs con (unariseArgs rho args)

------------------------
unariseExpr :: UniqSupply -> UnariseEnv -> StgExpr -> StgExpr
unariseExpr _ rho (StgApp f args)
  | null args
  , UbxTupleRep tys <- repType (idType f)
  =  -- Particularly important where (##) is concerned
     -- See Note [Nullary unboxed tuple]
    StgConApp (tupleDataCon Unboxed (length tys))
              (map StgVarArg (unariseId rho f))

  | otherwise
  = StgApp f (unariseArgs rho args)

unariseExpr _ _ (StgLit l)
  = StgLit l

unariseExpr _ rho (StgConApp dc args)
  | isUnboxedTupleCon dc = StgConApp (tupleDataCon Unboxed (length args')) args'
  | otherwise            = StgConApp dc args'
  where
    args' = unariseArgs rho args

unariseExpr _ rho (StgOpApp op args ty)
  = StgOpApp op (unariseArgs rho args) ty

unariseExpr us rho (StgLam xs e)
  = StgLam xs' (unariseExpr us' rho' e)
  where
    (us', rho', xs') = unariseIdBinders us rho xs

unariseExpr us rho (StgCase e case_lives alts_lives bndr srt alt_ty alts)
  = StgCase (unariseExpr us1 rho e) (unariseLives rho case_lives)
            (unariseLives rho alts_lives) bndr (unariseSRT rho srt)
            alt_ty alts'
 where
    (us1, us2) = splitUniqSupply us
    alts'      = unariseAlts us2 rho alt_ty bndr alts

unariseExpr us rho (StgLet bind e)
  = StgLet (unariseBinding us1 rho bind) (unariseExpr us2 rho e)
  where
    (us1, us2) = splitUniqSupply us

unariseExpr us rho (StgLetNoEscape live_in_let live_in_bind bind e)
  = StgLetNoEscape (unariseLives rho live_in_let) (unariseLives rho live_in_bind)
                   (unariseBinding us1 rho bind) (unariseExpr us2 rho e)
  where
    (us1, us2) = splitUniqSupply us

unariseExpr us rho (StgTick tick e)
  = StgTick tick (unariseExpr us rho e)

------------------------
unariseAlts :: UniqSupply -> UnariseEnv -> AltType -> Id -> [StgAlt] -> [StgAlt]
unariseAlts us rho (UbxTupAlt n) bndr [(DEFAULT, [], [], e)]
  = [(DataAlt (tupleDataCon Unboxed n), ys, uses, unariseExpr us2' rho' e)]
  where
    (us2', rho', ys) = unariseIdBinder us rho bndr
    uses = replicate (length ys) (not (isDeadBinder bndr))

unariseAlts us rho (UbxTupAlt n) bndr [(DataAlt _, ys, uses, e)]
  = [(DataAlt (tupleDataCon Unboxed n), ys', uses', unariseExpr us2' rho'' e)]
  where
    (us2', rho', ys', uses') = unariseUsedIdBinders us rho ys uses
    rho'' = extendVarEnv rho' bndr ys'

unariseAlts _ _ (UbxTupAlt _) _ alts
  = pprPanic "unariseExpr: strange unboxed tuple alts" (ppr alts)

unariseAlts us rho _ _ alts
  = zipWith (\us alt -> unariseAlt us rho alt) (listSplitUniqSupply us) alts

--------------------------
unariseAlt :: UniqSupply -> UnariseEnv -> StgAlt -> StgAlt
unariseAlt us rho (con, xs, uses, e)
  = (con, xs', uses', unariseExpr us' rho' e)
  where
    (us', rho', xs', uses') = unariseUsedIdBinders us rho xs uses

------------------------
unariseSRT :: UnariseEnv -> SRT -> SRT
unariseSRT _   NoSRT            = NoSRT
unariseSRT rho (SRTEntries ids) = SRTEntries (concatMapVarSet (unariseId rho) ids)

unariseLives :: UnariseEnv -> StgLiveVars -> StgLiveVars
unariseLives rho ids = concatMapVarSet (unariseId rho) ids

unariseArgs :: UnariseEnv -> [StgArg] -> [StgArg]
unariseArgs rho = concatMap (unariseArg rho)

unariseArg :: UnariseEnv -> StgArg -> [StgArg]
unariseArg rho (StgVarArg x) = map StgVarArg (unariseId rho x)
unariseArg _   (StgLitArg l) = [StgLitArg l]

unariseIds :: UnariseEnv -> [Id] -> [Id]
unariseIds rho = concatMap (unariseId rho)

unariseId :: UnariseEnv -> Id -> [Id]
unariseId rho x
  | Just ys <- lookupVarEnv rho x
  = ASSERT2( case repType (idType x) of UbxTupleRep _ -> True; _ -> False
           , text "unariseId: not unboxed tuple" <+> ppr x )
    ys

  | otherwise
  = ASSERT2( case repType (idType x) of UbxTupleRep _ -> False; _ -> True
           , text "unariseId: was unboxed tuple" <+> ppr x )
    [x]

unariseUsedIdBinders :: UniqSupply -> UnariseEnv -> [Id] -> [Bool]
                     -> (UniqSupply, UnariseEnv, [Id], [Bool])
unariseUsedIdBinders us rho xs uses
  = case mapAccumL2 do_one us rho (zipEqual "unariseUsedIdBinders" xs uses) of
      (us', rho', xs_usess) -> uncurry ((,,,) us' rho') (unzip (concat xs_usess))
  where
    do_one us rho (x, use) = third3 (map (flip (,) use)) (unariseIdBinder us rho x)

unariseIdBinders :: UniqSupply -> UnariseEnv -> [Id] -> (UniqSupply, UnariseEnv, [Id])
unariseIdBinders us rho xs = third3 concat $ mapAccumL2 unariseIdBinder us rho xs

unariseIdBinder :: UniqSupply -> UnariseEnv
                -> Id                -- Binder
                -> (UniqSupply,
                    UnariseEnv,      -- What to expand to at occurrence sites
                    [Id])            -- What to expand to at binding site
unariseIdBinder us rho x = case repType (idType x) of
    UnaryRep {} -> (us, rho, [x])

    UbxTupleRep tys
      | null tys  -> -- See Note [Unarisation and nullary tuples]
                     let ys = [voidPrimId]
                         rho' = extendVarEnv rho x ys
                     in (us, rho', [voidArgId])

      | otherwise -> let (us0, us1) = splitUniqSupply us
                         ys   = unboxedTupleBindersFrom us0 x tys
                         rho' = extendVarEnv rho x ys
                      in (us1, rho', ys)

unboxedTupleBindersFrom :: UniqSupply -> Id -> [UnaryType] -> [Id]
unboxedTupleBindersFrom us x tys = zipWith (mkSysLocalOrCoVar fs) (uniqsFromSupply us) tys
  where fs = occNameFS (getOccName x)

concatMapVarSet :: (Var -> [Var]) -> VarSet -> VarSet
concatMapVarSet f xs = mkVarSet [x' | x <- varSetElems xs, x' <- f x]
