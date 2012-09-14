%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%

Run through the STG code and compute the Static Reference Table for
each let-binding.  At the same time, we figure out which top-level
bindings have no CAF references, and record the fact in their IdInfo.

\begin{code}
module SRT( computeSRTs ) where

#include "HsVersions.h"

import StgSyn
import Id               ( Id )
import VarSet
import VarEnv
import Maybes           ( orElse, expectJust )
import Bitmap

import DynFlags
import Outputable

import Data.List
\end{code}

\begin{code}
computeSRTs :: DynFlags -> [StgBinding] -> [(StgBinding,[(Id,[Id])])]
  -- The incoming bindingd are filled with SRTEntries in their SRT slots
  -- the outgoing ones have NoSRT/SRT values instead

computeSRTs dflags binds = srtTopBinds dflags emptyVarEnv binds

-- --------------------------------------------------------------------------
-- Top-level Bindings

srtTopBinds :: DynFlags -> IdEnv Id -> [StgBinding] -> [(StgBinding, [(Id,[Id])])]

srtTopBinds _ _   [] = []
srtTopBinds dflags env (StgNonRec b rhs : binds) =
  (StgNonRec b rhs', [(b,srt')]) : srtTopBinds dflags env' binds
  where
    (rhs', srt) = srtTopRhs dflags b rhs
    env' = maybeExtendEnv env b rhs
    srt' = applyEnvList env srt
srtTopBinds dflags env (StgRec bs : binds) =
  (StgRec (zip bndrs rhss), zip bndrs srts') : srtTopBinds dflags env binds
  where
    (rhss, srts) = unzip [ srtTopRhs dflags b r | (b,r) <- bs ]
    bndrs = map fst bs
    srts' = map (applyEnvList env) srts

-- Shorting out indirections in SRTs:  if a binding has an SRT with a single
-- element in it, we just inline it with that element everywhere it occurs
-- in other SRTs.
--
-- This is in a way a generalisation of the CafInfo.  CafInfo says
-- whether a top-level binding has *zero* CAF references, allowing us
-- to omit it from SRTs.  Here, we pick up bindings with *one* CAF
-- reference, and inline its SRT everywhere it occurs.  We could pass
-- this information across module boundaries too, but we currently
-- don't.

maybeExtendEnv ::IdEnv Id -> Id -> StgRhs -> IdEnv Id
maybeExtendEnv env bndr (StgRhsClosure _ _ _ ReEntrant (SRTEntries cafs) _ _)
  | [one] <- varSetElems cafs
  = extendVarEnv env bndr (applyEnv env one)
maybeExtendEnv env _ _ = env

applyEnvList :: IdEnv Id -> [Id] -> [Id]
applyEnvList env = map (applyEnv env)

applyEnv :: IdEnv Id -> Id -> Id
applyEnv env id = lookupVarEnv env id `orElse` id

-- ----  Top-level right hand sides:

srtTopRhs :: DynFlags -> Id -> StgRhs -> (StgRhs, [Id])

srtTopRhs _ _ rhs@(StgRhsCon _ _ _) = (rhs, [])
srtTopRhs dflags _ rhs@(StgRhsClosure _ _ _ _  (SRTEntries cafs) _ _)
  = (srtRhs dflags table rhs, elems)
  where
        elems = varSetElems cafs
        table = mkVarEnv (zip elems [0..])
srtTopRhs _ _ (StgRhsClosure _ _ _ _  NoSRT _ _) = panic "srtTopRhs NoSRT"
srtTopRhs _ _ (StgRhsClosure _ _ _ _  (SRT _ _ _) _ _) = panic "srtTopRhs SRT"

-- ---- Binds:

srtBind :: DynFlags -> IdEnv Int -> StgBinding -> StgBinding

srtBind dflags table (StgNonRec binder rhs) = StgNonRec binder (srtRhs dflags table rhs)
srtBind dflags table (StgRec pairs) = StgRec [ (b, srtRhs dflags table r) | (b,r) <- pairs ]

-- ---- Right Hand Sides:

srtRhs :: DynFlags -> IdEnv Int -> StgRhs -> StgRhs

srtRhs _      _     e@(StgRhsCon _ _ _) = e
srtRhs dflags table (StgRhsClosure cc bi free_vars u srt args body)
  = StgRhsClosure cc bi free_vars u (constructSRT dflags table srt) args
        $! (srtExpr dflags table body)

-- ---------------------------------------------------------------------------
-- Expressions

srtExpr :: DynFlags -> IdEnv Int -> StgExpr -> StgExpr

srtExpr _ _ e@(StgApp _ _)       = e
srtExpr _ _ e@(StgLit _)         = e
srtExpr _ _ e@(StgConApp _ _)    = e
srtExpr _ _ e@(StgOpApp _ _ _)   = e

srtExpr dflags table (StgSCC cc tick push expr) = StgSCC cc tick push $! srtExpr dflags table expr

srtExpr dflags table (StgTick m n expr) = StgTick m n $! srtExpr dflags table expr

srtExpr dflags table (StgCase scrut live1 live2 uniq srt alt_type alts)
 = StgCase expr' live1 live2 uniq srt' alt_type alts'
 where
   expr' = srtExpr dflags table scrut
   srt'  = constructSRT dflags table srt
   alts' = map (srtAlt dflags table) alts

srtExpr dflags table (StgLet bind body)
  = srtBind dflags table bind =: \ bind' ->
    srtExpr dflags table body             =: \ body' ->
    StgLet bind' body'

srtExpr dflags table (StgLetNoEscape live1 live2 bind body)
  = srtBind dflags table bind =: \ bind' ->
    srtExpr dflags table body             =: \ body' ->
    StgLetNoEscape live1 live2 bind' body'

srtExpr _ _table expr = pprPanic "srtExpr" (ppr expr)

srtAlt :: DynFlags -> IdEnv Int -> StgAlt -> StgAlt
srtAlt dflags table (con,args,used,rhs)
  = (,,,) con args used $! srtExpr dflags table rhs

-----------------------------------------------------------------------------
-- Construct an SRT bitmap.

constructSRT :: DynFlags -> IdEnv Int -> SRT -> SRT
constructSRT dflags table (SRTEntries entries)
 | isEmptyVarSet entries = NoSRT
 | otherwise  = seqBitmap bitmap $ SRT offset len bitmap
  where
    ints = map (expectJust "constructSRT" . lookupVarEnv table)
                (varSetElems entries)
    sorted_ints = sort ints
    offset = head sorted_ints
    bitmap_entries = map (subtract offset) sorted_ints
    len = last bitmap_entries + 1
    bitmap = intsToBitmap dflags len bitmap_entries
constructSRT _ _ NoSRT = panic "constructSRT NoSRT"
constructSRT _ _ (SRT {}) = panic "constructSRT SRT"

-- ---------------------------------------------------------------------------
-- Misc stuff

(=:) :: a -> (a -> b) -> b
a =: k  = k a

\end{code}
