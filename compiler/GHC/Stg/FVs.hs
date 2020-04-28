{- |
Non-global free variable analysis on STG terms. This pass annotates
non-top-level closure bindings with captured variables. Global variables are not
captured. For example, in a top-level binding like (pseudo-STG)

    f = \[x,y] .
      let g = \[p] . reverse (x ++ p)
      in g y

In g, `reverse` and `(++)` are global variables so they're not considered free.
`p` is an argument, so `x` is the only actual free variable here. The annotated
version is thus:

    f = \[x,y] .
      let g = [x] \[p] . reverse (x ++ p)
      in g y

Note that non-top-level recursive bindings are also considered free within the
group:

    map = {} \r [f xs0]
      let {
        Rec {
          go = {f, go} \r [xs1]
            case xs1 of {
              [] -> [] [];
              : x xs2 ->
                  let { xs' = {go, xs2} \u [] go xs2; } in
                  let { x' = {f, x} \u [] f x; } in
                  : [x' xs'];
            };
        end Rec }
      } in go xs0;

Here go is free in its RHS.

Top-level closure bindings never capture variables as all of their free
variables are global.
-}
module GHC.Stg.FVs (
    annTopBindingsFreeVars,
    annBindingFreeVars
  ) where

import GhcPrelude

import GHC.Stg.Syntax
import GHC.Types.Id
import GHC.Types.Var.Set
import GHC.Core          ( Tickish(Breakpoint) )
import GHC.Core.DataCon  ( dataConRepType )
import GHC.Core.Type     ( isLiftedType_maybe )
import Outputable
import PrimOp
import Util

import Data.Maybe ( mapMaybe )

newtype Env
  = Env
  { locals :: IdSet
  }

emptyEnv :: Env
emptyEnv = Env emptyVarSet

addLocals :: [Id] -> Env -> Env
addLocals bndrs env
  = env { locals = extendVarSetList (locals env) bndrs }

-- | Annotates a top-level STG binding group with its free variables.
annTopBindingsFreeVars :: [StgTopBinding] -> [CgStgTopBinding]
annTopBindingsFreeVars = map go
  where
    go (StgTopStringLit id bs) = StgTopStringLit id bs
    go (StgTopLifted bind)
      = StgTopLifted (annBindingFreeVars bind)

-- | Annotates an STG binding with its free variables.
annBindingFreeVars :: StgBinding -> CgStgBinding
annBindingFreeVars = fst . binding False emptyEnv emptyDVarSet

boundIds :: StgBinding -> [Id]
boundIds (StgNonRec b _) = [b]
boundIds (StgRec pairs)  = map fst pairs

-- Note [Tracking local binders]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'locals' contains non-toplevel, non-imported binders.
-- We maintain the set in 'expr', 'alt' and 'rhs', which are the only
-- places where new local binders are introduced.
-- Why do it there rather than in 'binding'? Two reasons:
--
--   1. We call 'binding' from 'annTopBindingsFreeVars', which would
--      add top-level bindings to the 'locals' set.
--   2. In the let(-no-escape) case, we need to extend the environment
--      prior to analysing the body, but we also need the fvs from the
--      body to analyse the RHSs. No way to do this without some
--      knot-tying.

-- | This makes sure that only local, non-global free vars make it into the set.
mkFreeVarSet :: Env -> [Id] -> DIdSet
mkFreeVarSet env = mkDVarSet . filter (`elemVarSet` locals env)

args :: Env -> [StgArg] -> DIdSet
args env = mkFreeVarSet env . mapMaybe f
  where
    f (StgVarArg occ) = Just occ
    f _               = Nothing

binding :: Bool -> Env -> DIdSet -> StgBinding -> (CgStgBinding, DIdSet)
binding upstream_allocates env body_fv (StgNonRec bndr r) = (StgNonRec bndr r', fvs)
  where
    -- See Note [Tracking local binders]
    (r', rhs_fvs) = rhs upstream_allocates env r
    fvs = delDVarSet body_fv bndr `unionDVarSet` rhs_fvs
binding upstream_allocates env body_fv (StgRec pairs) = (StgRec pairs', fvs)
  where
    -- See Note [Tracking local binders]
    bndrs = map fst pairs
    (rhss, rhs_fvss) = mapAndUnzip (rhs upstream_allocates env . snd) pairs
    pairs' = zip bndrs rhss
    fvs = delDVarSetList (unionDVarSets (body_fv:rhs_fvss)) bndrs

expr :: Bool -> Env -> StgExpr -> (CgStgExpr, DIdSet)
expr upstream_allocates env = go upstream_allocates
  where
    go _ (StgApp occ as)
      = (StgApp occ as, unionDVarSet (args env as) (mkFreeVarSet env [occ]))
    go _ (StgLit lit) = (StgLit lit, emptyDVarSet)
    go _ (StgConApp dc as tys) = (StgConApp dc as tys, args env as)
    go _ (StgOpApp op as ty) = (StgOpApp op as ty, args env as)
    go _ StgLam{} = pprPanic "StgFVs: StgLam" empty
    go upstream_allocates (StgCase scrut bndr ty _ alts)
      = (StgCase scrut' bndr ty hc alts', fvs)
      where
        -- Note [Computing StgCaseGcFlag]
        --  * If the scrutinee <scrut> requires any non-trivial work, we MUST GcInAlts.
        --    For example if <scrut> was (g x), then calling g might result in lots of
        --    allocation, so any heap check done at the start of f is irrelevant to the
        --    branches. They must do their own checks.
        --  * If there is just one alternative, then it's always good to amalgamate
        --  * If there is heap allocation in the code before the case, then we are going
        --    to do a heap-check upstream anyway. In that case, don't do one in the
        --    alterantives too.
        --  * Otherwise, if there no heap allocation upstream, put heap checks in each
        --    alternative. The reasoning here was that if one alternative needs heap and
        --    the other one  doesn't we don't want to pay the runtime for the heap check
        --    in the case where the heap-free alternative is taken.
        hc | stgExprMayBlockOrAllocate scrut = HeapCheckInAlts
           | isSingleton alts                = HeapCheckUpstream
           | upstream_allocates              = HeapCheckUpstream
           | otherwise                       = HeapCheckInAlts
        upstream_allocates'
           | HeapCheckInAlts <- hc = False
           | otherwise             = upstream_allocates
        (scrut', scrut_fvs) = go upstream_allocates' scrut
        -- See Note [Tracking local binders]
        (alts', alt_fvss)
          = mapAndUnzip (alt upstream_allocates' (addLocals [bndr] env)) alts
        alt_fvs = unionDVarSets alt_fvss
        fvs = delDVarSet (unionDVarSet scrut_fvs alt_fvs) bndr
    go upstream_allocates (StgLet ext bind body)
      = go_bind upstream_allocates (StgLet ext) bind body
    go upstream_allocates (StgLetNoEscape ext bind body)
      = go_bind upstream_allocates (StgLetNoEscape ext) bind body
    go upstream_allocates (StgTick tick e) = (StgTick tick e', fvs')
      where
        (e', fvs) = go upstream_allocates e
        fvs' = unionDVarSet (tickish tick) fvs
        tickish (Breakpoint _ ids) = mkDVarSet ids
        tickish _                  = emptyDVarSet

    go_bind upstream_allocates dc bind body = (dc bind' body', fvs)
      where
        -- See Note [Tracking local binders]
        env' = addLocals (boundIds bind) env
        (body', body_fvs) = expr True env' body
        (bind', fvs) = binding upstream_allocates env' body_fvs bind

rhs :: Bool -> Env -> StgRhs -> (CgStgRhs, DIdSet)
rhs upstream_allocates env (StgRhsClosure _ ccs uf bndrs body)
  = (StgRhsClosure fvs ccs uf bndrs body', fvs)
  where
    -- See Note [Tracking local binders]
    (body', body_fvs) = expr upstream_allocates (addLocals bndrs env) body
    fvs = delDVarSetList body_fvs bndrs
rhs _ env (StgRhsCon ccs dc as) = (StgRhsCon ccs dc as, args env as)

alt :: Bool -> Env -> StgAlt -> (CgStgAlt, DIdSet)
alt upstream_allocates env (con, bndrs, e) = ((con, bndrs, e'), fvs)
  where
    -- See Note [Tracking local binders]
    (e', rhs_fvs) = expr upstream_allocates (addLocals bndrs env) e
    fvs = delDVarSetList rhs_fvs bndrs

-- | ...
stgExprMayBlockOrAllocate :: StgExpr -> Bool
stgExprMayBlockOrAllocate (StgLet _ _ _) = True
stgExprMayBlockOrAllocate (StgLetNoEscape _ _ body)
  = stgExprMayBlockOrAllocate body
stgExprMayBlockOrAllocate (StgConApp dataCon args _)
  | Just True <- isLiftedType_maybe (dataConRepType dataCon)
  , not (null args) = True
stgExprMayBlockOrAllocate (StgOpApp (StgPrimOp op) _ _) = primOpMayBlockOrAllocate op
stgExprMayBlockOrAllocate (StgTick _ e) = stgExprMayBlockOrAllocate e
stgExprMayBlockOrAllocate (StgCase scrut _ alt_type _ alts)
  | stgExprMayBlockOrAllocate scrut = True
  | not (isSimpleScrut scrut alt_type) = False
  | otherwise = any ( \(_, _, rhs) -> stgExprMayBlockOrAllocate rhs ) alts
stgExprMayBlockOrAllocate _ = False

primOpMayBlockOrAllocate :: PrimOp -> Bool
primOpMayBlockOrAllocate p = case p of
-- NoDuplicateOp = ... blocking?
  NewByteArrayOp_Char -> True -- calls MAYBE_GC_N
  NewPinnedByteArrayOp_Char  -> True -- calls MAYBE_GC_N
  NewAlignedPinnedByteArrayOp_Char  -> True -- calls MAYBE_GC
  ResizeMutableByteArrayOp_Char  -> True -- calls stg_newByteArrayzh
  NewArrayOp -> True -- calls MAYBE_GC
  CloneArrayOp  -> True -- calls cloneArray which may call MAYBE_GC
  CloneMutableArrayOp -> True -- calls cloneArray which may call MAYBE_GC
  FreezeArrayOp -> True -- calls cloneArray which may call MAYBE_GC
  ThawArrayOp -> True -- calls cloneArray which may call MAYBE_GC
  NewArrayArrayOp -> True -- calls MAYBE_GC_N
  NewSmallArrayOp -> True -- calls MAYBE_GC
  UnsafeThawArrayOp -> True -- Not sure here... Calls recordMutable -> recordMutableCap -> allocBlock_lock()
  UnsafeThawSmallArrayOp -> True -- Not sure here... Calls recordMutable -> recordMutableCap -> allocBlock_lock()
  CloneSmallArrayOp -> True -- calls cloneSmallArray which may call MAYBE_GC
  CloneSmallMutableArrayOp -> True -- calls cloneSmallArray which may call MAYBE_GC
  FreezeSmallArrayOp -> True -- calls cloneSmallArray which may call MAYBE_GC
  ThawSmallArrayOp -> True -- calls cloneSmallArray which may call MAYBE_GC
  NewMutVarOp -> True -- calls ALLOC_PRIM_P
  AtomicModifyMutVar2Op -> True -- calls HP_CHK_GEN_TICKY
  AtomicModifyMutVar_Op -> True -- calls HP_CHK_GEN_TICKY
  MkWeakOp -> True -- calls ALLOC_PRIM that calls HP_CHK_GEN_TICKY
  AddCFinalizerToWeakOp -> True -- calls ALLOC_PRIM that calls HP_CHK_GEN_TICKY
  FloatDecode_IntOp -> True -- calls STK_CHK_GEN_N
  DoubleDecode_2IntOp -> True -- calls STK_CHK_GEN_N
  DoubleDecode_Int64Op -> True -- calls STK_CHK_GEN_N
  ForkOp -> True -- calls MAYBE_GC_P
  ForkOnOp -> True -- calls MAYBE_GC
  AtomicallyOp -> True -- calls MAYBE_GC_P (and STK_CHK_GEN)
  CatchSTMOp -> True -- STK_CHK_GEN
  CatchRetryOp -> True -- calls MAYBE_GC_PP (and STK_CHK_GEN)
  RetryOp -> True -- calls MAYBE_GC_
  NewTVarOp -> True -- calls ALLOC_PRIM_P
  ReadTVarOp -> True -- calls MAYBE_GC_P.. how about stg_readTVarIO?
  WriteTVarOp -> True -- calls MAYBE_GC_PP
  NewMVarOp -> True -- calls ALLOC_PRIM_
  TakeMVarOp -> True -- calls ALLOC_PRIM_WITH_CUSTOM_FAILURE (also is blocking)
  PutMVarOp -> True -- calls ALLOC_PRIM_WITH_CUSTOM_FAILURE (also is blocking)
  ReadMVarOp -> True -- calls ALLOC_PRIM_WITH_CUSTOM_FAILURE and GC_PRIM_P (also is blocking)
  MakeStableNameOp -> True -- calls MAYBE_GC_P
  NewBCOOp -> True -- calls ALLOC_PRIM
  MkApUpd0_Op -> True -- calls HP_CHK_P
  UnpackClosureOp -> True -- calls ALLOC_PRIM_P
  WaitReadOp -> True -- blocking
  WaitWriteOp -> True -- blocking
  DelayOp -> True -- blocking
  _ -> False

-- borrowed from GHC.StgToCmm.Expr
isSimpleScrut :: StgExpr -> AltType -> Bool
isSimpleScrut (StgOpApp (StgPrimOp op) _ _) _ = primOpMayBlockOrAllocate op
isSimpleScrut (StgLit _) _ = True
isSimpleScrut (StgApp _ []) (PrimAlt _) = True
isSimpleScrut _  _ = False
