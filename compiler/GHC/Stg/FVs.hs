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
annBindingFreeVars = fst . binding emptyEnv emptyDVarSet

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

binding :: Env -> DIdSet -> StgBinding -> (CgStgBinding, DIdSet)
binding env body_fv (StgNonRec bndr r) = (StgNonRec bndr r', fvs)
  where
    -- See Note [Tracking local binders]
    (r', rhs_fvs) = rhs env r
    fvs = delDVarSet body_fv bndr `unionDVarSet` rhs_fvs
binding env body_fv (StgRec pairs) = (StgRec pairs', fvs)
  where
    -- See Note [Tracking local binders]
    bndrs = map fst pairs
    (rhss, rhs_fvss) = mapAndUnzip (rhs env . snd) pairs
    pairs' = zip bndrs rhss
    fvs = delDVarSetList (unionDVarSets (body_fv:rhs_fvss)) bndrs

expr :: Env -> StgExpr -> (CgStgExpr, DIdSet)
expr env = go
  where
    go (StgApp occ as)
      = (StgApp occ as, unionDVarSet (args env as) (mkFreeVarSet env [occ]))
    go (StgLit lit) = (StgLit lit, emptyDVarSet)
    go (StgConApp dc as tys) = (StgConApp dc as tys, args env as)
    go (StgOpApp op as ty) = (StgOpApp op as ty, args env as)
    go StgLam{} = pprPanic "StgFVs: StgLam" empty
    go (StgCase scrut bndr ty _ alts) = (StgCase scrut' bndr ty do_gc alts', fvs)
      where
        (scrut', scrut_fvs) = go scrut
        -- See Note [Tracking local binders]
        (alts', alt_fvss) = mapAndUnzip (alt (addLocals [bndr] env)) alts
        alt_fvs = unionDVarSets alt_fvss
        fvs = delDVarSet (unionDVarSet scrut_fvs alt_fvs) bndr
        -- See Note [Case alternative allocation strategy]
        is_cmp_op (StgOpApp (StgPrimOp op) _ _) = isComparisonPrimOp op
        is_cmp_op _                             = False
        simple_scrut = isSimpleScrut scrut ty
        -- should we use this to construct do_gc?
        -- alts_allocate = any ( \(_, _, rhs) -> stgExprAllocates rhs ) alts
        do_gc
          | is_cmp_op scrut  = False  -- See Note [GC for conditionals]
          | not simple_scrut = True
          | isSingleton alts = False
          | otherwise        = True
    go (StgLet ext bind body) = go_bind (StgLet ext) bind body
    go (StgLetNoEscape ext bind body) = go_bind (StgLetNoEscape ext) bind body
    go (StgTick tick e) = (StgTick tick e', fvs')
      where
        (e', fvs) = go e
        fvs' = unionDVarSet (tickish tick) fvs
        tickish (Breakpoint _ ids) = mkDVarSet ids
        tickish _                  = emptyDVarSet

    go_bind dc bind body = (dc bind' body', fvs)
      where
        -- See Note [Tracking local binders]
        env' = addLocals (boundIds bind) env
        (body', body_fvs) = expr env' body
        (bind', fvs) = binding env' body_fvs bind

rhs :: Env -> StgRhs -> (CgStgRhs, DIdSet)
rhs env (StgRhsClosure _ ccs uf bndrs body)
  = (StgRhsClosure fvs ccs uf bndrs body', fvs)
  where
    -- See Note [Tracking local binders]
    (body', body_fvs) = expr (addLocals bndrs env) body
    fvs = delDVarSetList body_fvs bndrs
rhs env (StgRhsCon ccs dc as) = (StgRhsCon ccs dc as, args env as)

alt :: Env -> StgAlt -> (CgStgAlt, DIdSet)
alt env (con, bndrs, e) = ((con, bndrs, e'), fvs)
  where
    -- See Note [Tracking local binders]
    (e', rhs_fvs) = expr (addLocals bndrs env) e
    fvs = delDVarSetList rhs_fvs bndrs

-- | TODO: Note
stgExprAllocates :: StgExpr -> Bool
stgExprAllocates (StgLet _ _ _) = True
stgExprAllocates (StgLetNoEscape _ _ body) 
  = stgExprAllocates body
stgExprAllocates (StgConApp dataCon args _) 
  | Just True <- isLiftedType_maybe (dataConRepType dataCon)
  , not (null args) = True
stgExprAllocates (StgOpApp (StgPrimOp op) _ _) = primOpCanGC op
stgExprAllocates (StgTick _ e) = stgExprAllocates e
stgExprAllocates (StgCase scrut _ alt_type _ alts)
  | stgExprAllocates scrut = True
  | not (isSimpleScrut scrut alt_type) = False
  | any ( \(_, _, rhs) -> stgExprAllocates rhs ) alts = True
  | otherwise = False
stgExprAllocates _ = False

primOpCanGC :: PrimOp -> Bool
-- NoDuplicateOp = ... blocking?
primOpCanGC NewByteArrayOp_Char = True -- calls MAYBE_GC_N
primOpCanGC NewPinnedByteArrayOp_Char = True -- calls MAYBE_GC_N
primOpCanGC NewAlignedPinnedByteArrayOp_Char = True -- calls MAYBE_GC
primOpCanGC ResizeMutableByteArrayOp_Char = True -- calls stg_newByteArrayzh
primOpCanGC NewArrayOp = True -- calls MAYBE_GC
primOpCanGC CloneArrayOp = True -- calls cloneArray which may call MAYBE_GC
primOpCanGC CloneMutableArrayOp = True -- calls cloneArray which may call MAYBE_GC
primOpCanGC FreezeArrayOp = True -- calls cloneArray which may call MAYBE_GC
primOpCanGC ThawArrayOp = True -- calls cloneArray which may call MAYBE_GC
primOpCanGC NewArrayArrayOp = True -- calls MAYBE_GC_N
primOpCanGC NewSmallArrayOp = True -- calls MAYBE_GC
primOpCanGC UnsafeThawArrayOp = True -- Not sure here... Calls recordMutable -> recordMutableCap -> allocBlock_lock()
primOpCanGC UnsafeThawSmallArrayOp = True -- Not sure here... Calls recordMutable -> recordMutableCap -> allocBlock_lock()
primOpCanGC CloneSmallArrayOp = True -- calls cloneSmallArray which may call MAYBE_GC
primOpCanGC CloneSmallMutableArrayOp = True -- calls cloneSmallArray which may call MAYBE_GC
primOpCanGC FreezeSmallArrayOp = True -- calls cloneSmallArray which may call MAYBE_GC
primOpCanGC ThawSmallArrayOp = True -- calls cloneSmallArray which may call MAYBE_GC
primOpCanGC NewMutVarOp = True -- calls ALLOC_PRIM_P
primOpCanGC AtomicModifyMutVar2Op = True -- calls HP_CHK_GEN_TICKY
primOpCanGC AtomicModifyMutVar_Op = True -- calls HP_CHK_GEN_TICKY
primOpCanGC MkWeakOp = True -- calls ALLOC_PRIM that calls HP_CHK_GEN_TICKY
primOpCanGC AddCFinalizerToWeakOp = True -- calls ALLOC_PRIM that calls HP_CHK_GEN_TICKY
primOpCanGC FloatDecode_IntOp = True -- calls STK_CHK_GEN_N
primOpCanGC DoubleDecode_2IntOp = True -- calls STK_CHK_GEN_N
primOpCanGC DoubleDecode_Int64Op = True -- calls STK_CHK_GEN_N
primOpCanGC ForkOp = True -- calls MAYBE_GC_P
primOpCanGC ForkOnOp = True -- calls MAYBE_GC
primOpCanGC AtomicallyOp = True -- calls MAYBE_GC_P (and STK_CHK_GEN)
primOpCanGC CatchSTMOp = True -- STK_CHK_GEN
primOpCanGC CatchRetryOp = True -- calls MAYBE_GC_PP (and STK_CHK_GEN)
primOpCanGC RetryOp = True -- calls MAYBE_GC_
primOpCanGC NewTVarOp = True -- calls ALLOC_PRIM_P
primOpCanGC ReadTVarOp = True -- calls MAYBE_GC_P.. how about stg_readTVarIO?
primOpCanGC WriteTVarOp = True -- calls MAYBE_GC_PP
primOpCanGC NewMVarOp = True -- calls ALLOC_PRIM_
primOpCanGC TakeMVarOp = True -- calls ALLOC_PRIM_WITH_CUSTOM_FAILURE (also is blocking)
primOpCanGC PutMVarOp = True -- calls ALLOC_PRIM_WITH_CUSTOM_FAILURE (also is blocking)
primOpCanGC ReadMVarOp = True -- calls ALLOC_PRIM_WITH_CUSTOM_FAILURE and GC_PRIM_P (also is blocking)
primOpCanGC MakeStableNameOp = True -- calls MAYBE_GC_P
primOpCanGC NewBCOOp = True -- calls ALLOC_PRIM
primOpCanGC MkApUpd0_Op = True -- calls HP_CHK_P
primOpCanGC UnpackClosureOp = True -- calls ALLOC_PRIM_P
primOpCanGC WaitReadOp = True -- blocking
primOpCanGC WaitWriteOp = True -- blocking
primOpCanGC DelayOp = True -- blocking
primOpCanGC _ = False

-- borrowed from GHC.StgToCmm.Expr
isSimpleScrut :: StgExpr -> AltType -> Bool
isSimpleScrut (StgOpApp (StgPrimOp op) _ _) _ = primOpCanGC op
isSimpleScrut (StgLit _) _ = True
isSimpleScrut (StgApp _ []) (PrimAlt _) = True
isSimpleScrut _  _ = False


