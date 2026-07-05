{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Reuse the scrutinised value for record updates that turn out to be
-- identities at runtime. See Note [Identity record updates].
module GHC.Stg.IdentityUpdate ( stgIdentityUpdates ) where

import GHC.Prelude

import GHC.Builtin.PrimOps ( PrimOp(ReallyUnsafePtrEqualityOp) )
import GHC.Builtin.Types.Prim ( intPrimTy )
import GHC.Core ( AltCon(..) )
import GHC.Core.DataCon
import GHC.Core.TyCon ( PrimRep(..) )
import GHC.Data.FastString ( fsLit )
import GHC.Data.Maybe ( firstJusts )
import GHC.Stg.Syntax
import GHC.Stg.Utils ( bindersOf )
import GHC.Types.Id
import GHC.Types.Literal ( mkLitIntUnchecked )
import GHC.Types.Unique.Supply
import GHC.Core.Multiplicity ( pattern ManyTy )
import GHC.Types.Var.Set
import GHC.Utils.Misc ( equalLength )

import Control.Monad ( mapAndUnzipM )

{- Note [Identity record updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A record update whose new field value is a nullary constructor, e.g.

    set_f1 arg = arg { f1 = B }

compiles to STG that scrutinises the record and unconditionally allocates
a copy:

    case arg of wild
      MkRecord _ f2 f3 f4 -> MkRecord [B f2 f3 f4]

If the field already contains B, the update is an identity and the
allocation is wasted (#21394). This pass rewrites such constructor
applications to first compare the old field against the new value:

    case arg of wild
      MkRecord f1 f2 f3 f4 ->
        case reallyUnsafePtrEquality# [B f1] of
          1#      -> wild
          DEFAULT -> MkRecord [B f2 f3 f4]

reallyUnsafePtrEquality# does not force its arguments, so unlike a
user-written comparison this never enters the old field's closure. The
check is sound because a nullary constructor has a unique static closure:
if the field's (tagged) pointer is bit-identical to the new value's, the
field is already that constructor and the scrutinised value 'wild' is
interchangeable with the freshly allocated copy. If the field points
elsewhere (a thunk, an indirection, an untagged reference) the comparison
fails and we merely allocate as before, so a miss is only a few
instructions of overhead while a hit saves the whole allocation.

We fire only when

  * the constructor application appears in an alternative scrutinising
    the same constructor, and all arguments except exactly one are the
    corresponding field binders (so the update really is an identity but
    for that field), and

  * the one differing argument is a nullary data constructor worker, so
    the pointer comparison against its static closure is meaningful, and

  * the old field was not itself scrutinised by an enclosing case; in
    that situation the user already wrote the check by hand and on this
    code path the comparison could only fail.

Like StgCSE (see Note [Free variables of an StgClosure] in GHC.Stg.CSE)
we do not reuse a case binder from outside an enclosing closure: that
would grow the closure's free-variable set and retain the record for
longer than the original program would.

The pass runs after unarisation; unboxed tuple and sum constructors are
never candidates because their "case binders" are not in scope (see
Note [Post-unarisation invariants] in GHC.Stg.Unarise), and their fields
are not all boxed anyway.
-}

data IUEnv = IUEnv
    { iu_entries :: [(DataCon, Id, [Id])]
        -- ^ Enclosing case alternatives, innermost first: the scrutinised
        --   constructor, the case binder, and the field binders.
    , iu_checked :: IdSet
        -- ^ Variables scrutinised by an enclosing case; see
        --   Note [Identity record updates].
    }

emptyIUEnv :: IUEnv
emptyIUEnv = IUEnv [] emptyVarSet

-- | Drop entries mentioning any of the given (re-)bound variables.
shadowBndrs :: [Id] -> IUEnv -> IUEnv
shadowBndrs bndrs env
  = env { iu_entries = filter ok (iu_entries env) }
  where
    bndr_set = mkVarSet bndrs
    ok (_, case_bndr, flds)
      = not (case_bndr `elemVarSet` bndr_set)
        && not (any (`elemVarSet` bndr_set) flds)

stgIdentityUpdates :: UniqSupply -> [StgTopBinding] -> [StgTopBinding]
stgIdentityUpdates us binds = initUs_ us (mapM go_top binds)
  where
    go_top (StgTopLifted bind) = StgTopLifted . fst <$> go_bind emptyIUEnv bind
    go_top t@(StgTopStringLit{}) = pure t

-- The returned IdSet contains case and field binders that the rewrite
-- revived; their occurrence info must be zapped so that codegen does not
-- apply the dead-binder optimisation to them.
-- See Note [Dead-binder optimisation] in GHC.StgToCmm.Expr.
go_bind :: IUEnv -> StgBinding -> UniqSM (StgBinding, IdSet)
go_bind env (StgNonRec b rhs) = do
    (rhs', revived) <- go_rhs env rhs
    pure (StgNonRec b rhs', revived)
go_bind env (StgRec pairs) = do
    (pairs', reviveds) <- mapAndUnzipM (\(b, rhs) -> do
                            (rhs', revived) <- go_rhs env rhs
                            pure ((b, rhs'), revived)) pairs
    pure (StgRec pairs', unionVarSets reviveds)

go_rhs :: IUEnv -> StgRhs -> UniqSM (StgRhs, IdSet)
go_rhs _ (StgRhsClosure ext ccs upd args body typ) = do
    -- Do not reuse case binders across a closure boundary; see
    -- Note [Identity record updates].
    (body', revived) <- go_expr emptyIUEnv body
    pure (StgRhsClosure ext ccs upd args body' typ, revived)
go_rhs _ rhs@(StgRhsCon{}) = pure (rhs, emptyVarSet)

go_expr :: IUEnv -> StgExpr -> UniqSM (StgExpr, IdSet)
go_expr _ e@(StgApp{}) = pure (e, emptyVarSet)
go_expr _ e@(StgLit{}) = pure (e, emptyVarSet)
go_expr _ e@(StgOpApp{}) = pure (e, emptyVarSet)
go_expr env (StgTick t e) = do
    (e', revived) <- go_expr env e
    pure (StgTick t e', revived)
go_expr env (StgLet ext bind body) = do
    (bind', r1) <- go_bind env bind
    (body', r2) <- go_expr (shadowBndrs (bindersOf bind) env) body
    pure (StgLet ext bind' body', r1 `unionVarSet` r2)
go_expr env (StgLetNoEscape ext bind body) = do
    (bind', r1) <- go_bind env bind
    (body', r2) <- go_expr (shadowBndrs (bindersOf bind) env) body
    pure (StgLetNoEscape ext bind' body', r1 `unionVarSet` r2)
go_expr env (StgCase scrut bndr ty alts) = do
    (scrut', r1) <- go_expr env scrut
    let env1 = shadowBndrs [bndr] env
        env2 | StgApp v [] <- scrut
             = env1 { iu_checked = extendVarSet (iu_checked env1) v }
             | otherwise
             = env1
    (alts', rs) <- mapAndUnzipM (go_alt env2 bndr ty) alts
    let revived = unionVarSets (r1:rs)
        bndr' | bndr `elemVarSet` revived = zapIdOccInfo bndr
              | otherwise                 = bndr
    pure (StgCase scrut' bndr' ty alts', revived)
go_expr env (StgConApp dc n args tys)
  | Just (case_bndr, old_fld, new_val) <- findReuse env dc args
  = do  eq_uniq <- getUniqueM
        let eq_bndr = mkSysLocal (fsLit "ipe") eq_uniq ManyTy intPrimTy
            ptr_eq  = StgOpApp (StgPrimOp ReallyUnsafePtrEqualityOp)
                               [StgVarArg new_val, StgVarArg old_fld]
                               intPrimTy
            alts = [ GenStgAlt DEFAULT [] (StgConApp dc n args tys)
                   , GenStgAlt (LitAlt (mkLitIntUnchecked 1)) []
                               (StgApp case_bndr [])
                   ]
        pure ( StgCase ptr_eq eq_bndr (PrimAlt IntRep) alts
             , mkVarSet [case_bndr, old_fld] )
  | otherwise
  = pure (StgConApp dc n args tys, emptyVarSet)

go_alt :: IUEnv -> Id -> AltType -> StgAlt -> UniqSM (StgAlt, IdSet)
go_alt env case_bndr ty (GenStgAlt con bndrs rhs) = do
    let env1 = shadowBndrs bndrs env
        env2 | DataAlt dc <- con
             , stgCaseBndrInScope ty True  -- this pass runs after unarise
             , not (isUnboxedTupleDataCon dc || isUnboxedSumDataCon dc)
             = env1 { iu_entries = (dc, case_bndr, bndrs) : iu_entries env1 }
             | otherwise
             = env1
    (rhs', revived) <- go_expr env2 rhs
    let bndrs' = [ if b `elemVarSet` revived then zapIdOccInfo b else b
                 | b <- bndrs ]
    pure (GenStgAlt con bndrs' rhs', revived)

-- | Does this constructor application reproduce an enclosing case
-- alternative's fields, except for exactly one field whose new value is a
-- nullary constructor? Returns the case binder to reuse, the old field
-- binder and the new value.
findReuse :: IUEnv -> DataCon -> [StgArg] -> Maybe (Id, Id, Id)
findReuse env dc args = firstJusts (map match (iu_entries env))
  where
    match (dc', case_bndr, flds)
      | dc' == dc
      , equalLength flds args
      , [(old_fld, StgVarArg new_val)] <- filter differs (zip flds args)
      , Just con <- isDataConWorkId_maybe new_val
      , dataConRepArity con == 0
      , not (old_fld `elemVarSet` iu_checked env)
      = Just (case_bndr, old_fld, new_val)
      | otherwise
      = Nothing

    differs (fld, StgVarArg v) = v /= fld
    differs (_,   StgLitArg{}) = True
