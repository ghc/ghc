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

import Outputable

import Data.List
\end{code}

\begin{code}
computeSRTs :: [StgBinding] -> [(StgBinding,[(Id,[Id])])]
  -- The incoming bindingd are filled with SRTEntries in their SRT slots
  -- the outgoing ones have NoSRT/SRT values instead

computeSRTs binds = srtTopBinds emptyVarEnv binds

-- --------------------------------------------------------------------------
-- Top-level Bindings

srtTopBinds :: IdEnv Id -> [StgBinding] -> [(StgBinding, [(Id,[Id])])]

srtTopBinds _   [] = []
srtTopBinds env (StgNonRec b rhs : binds) =
  (StgNonRec b rhs', [(b,srt')]) : srtTopBinds env' binds
  where
    (rhs', srt) = srtTopRhs b rhs
    env' = maybeExtendEnv env b rhs
    srt' = applyEnvList env srt
srtTopBinds env (StgRec bs : binds) =
  (StgRec (zip bndrs rhss), zip bndrs srts') : srtTopBinds env binds
  where
    (rhss, srts) = unzip [ srtTopRhs b r | (b,r) <- bs ]
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

srtTopRhs :: Id -> StgRhs -> (StgRhs, [Id])

srtTopRhs _ rhs@(StgRhsCon _ _ _) = (rhs, [])
srtTopRhs _ rhs@(StgRhsClosure _ _ _ _  (SRTEntries cafs) _ _)
  = (srtRhs table rhs, elems)
  where
        elems = varSetElems cafs
        table = mkVarEnv (zip elems [0..])
srtTopRhs _ (StgRhsClosure _ _ _ _  NoSRT _ _) = panic "srtTopRhs NoSRT"
srtTopRhs _ (StgRhsClosure _ _ _ _  (SRT _ _ _) _ _) = panic "srtTopRhs SRT"

-- ---- Binds:

srtBind :: IdEnv Int -> StgBinding -> StgBinding

srtBind table (StgNonRec binder rhs) = StgNonRec binder (srtRhs table rhs)
srtBind table (StgRec pairs) = StgRec [ (b, srtRhs table r) | (b,r) <- pairs ]

-- ---- Right Hand Sides:

srtRhs :: IdEnv Int -> StgRhs -> StgRhs

srtRhs _     e@(StgRhsCon _ _ _) = e
srtRhs table (StgRhsClosure cc bi free_vars u srt args body)
  = StgRhsClosure cc bi free_vars u (constructSRT table srt) args
        $! (srtExpr table body)

-- ---------------------------------------------------------------------------
-- Expressions

srtExpr :: IdEnv Int -> StgExpr -> StgExpr

srtExpr _ e@(StgApp _ _)       = e
srtExpr _ e@(StgLit _)         = e
srtExpr _ e@(StgConApp _ _)    = e
srtExpr _ e@(StgOpApp _ _ _)   = e

srtExpr table (StgSCC cc tick push expr) = StgSCC cc tick push $! srtExpr table expr

srtExpr table (StgTick m n expr) = StgTick m n $! srtExpr table expr

srtExpr table (StgCase scrut live1 live2 uniq srt alt_type alts)
 = StgCase expr' live1 live2 uniq srt' alt_type alts'
 where
   expr' = srtExpr table scrut
   srt'  = constructSRT table srt
   alts' = map (srtAlt table) alts

srtExpr table (StgLet bind body)
  = srtBind table bind =: \ bind' ->
    srtExpr table body             =: \ body' ->
    StgLet bind' body'

srtExpr table (StgLetNoEscape live1 live2 bind body)
  = srtBind table bind =: \ bind' ->
    srtExpr table body             =: \ body' ->
    StgLetNoEscape live1 live2 bind' body'

srtExpr _table expr = pprPanic "srtExpr" (ppr expr)

srtAlt :: IdEnv Int -> StgAlt -> StgAlt
srtAlt table (con,args,used,rhs)
  = (,,,) con args used $! srtExpr table rhs

-----------------------------------------------------------------------------
-- Construct an SRT bitmap.

constructSRT :: IdEnv Int -> SRT -> SRT
constructSRT table (SRTEntries entries)
 | isEmptyVarSet entries = NoSRT
 | otherwise  = seqBitmap bitmap $ SRT offset len bitmap
  where
    ints = map (expectJust "constructSRT" . lookupVarEnv table)
                (varSetElems entries)
    sorted_ints = sort ints
    offset = head sorted_ints
    bitmap_entries = map (subtract offset) sorted_ints
    len = last bitmap_entries + 1
    bitmap = intsToBitmap len bitmap_entries
constructSRT _ NoSRT = panic "constructSRT NoSRT"
constructSRT _ (SRT {}) = panic "constructSRT SRT"

-- ---------------------------------------------------------------------------
-- Misc stuff

(=:) :: a -> (a -> b) -> b
a =: k  = k a

\end{code}
