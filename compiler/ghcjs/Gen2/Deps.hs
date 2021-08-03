
module Gen2.Deps where

import           Id
import           StgSyn
import           VarSet
import           Data.List.NonEmpty (toList)
import Prelude

type LiveVars = DVarSet

liveStatic :: LiveVars -> LiveVars
liveStatic = filterDVarSet isGlobalId

liveVars :: LiveVars -> LiveVars
liveVars = filterDVarSet (not . isGlobalId)

stgTopBindLive :: StgTopBinding -> [(Id, LiveVars)]
stgTopBindLive (StgTopLifted b)   = stgBindLive b
stgTopBindLive StgTopStringLit {} = []

stgBindLive :: StgBinding -> [(Id, LiveVars)]
stgBindLive (StgNonRec b rhs) = [(b, stgRhsLive rhs)]
stgBindLive (StgRec bs) = map (\(b,rhs) -> (b, stgRhsLive rhs)) bs

stgBindRhsLive :: StgBinding -> LiveVars
stgBindRhsLive b =
  let (bs, ls) = unzip (stgBindLive b)
  in  delDVarSetList (unionDVarSets ls) bs

stgRhsLive :: StgRhs -> LiveVars
stgRhsLive (StgRhsClosure _ _ _ args e) =
  delDVarSetList (stgExprLive True e) args
stgRhsLive (StgRhsCon _ _ args) =
  mconcat (map stgArgLive args)

stgArgLive :: StgArg -> LiveVars
stgArgLive (StgVarArg occ) = unitDVarSet occ
stgArgLive  StgLitArg {}   = mempty

stgExprLive :: Bool -> StgExpr -> LiveVars
stgExprLive  _ (StgApp occ args) =
  unitDVarSet occ <> mconcat (map stgArgLive args)
stgExprLive _ StgLit {} =
  mempty
stgExprLive _ (StgConApp _dc args _tys) =
  mconcat (map stgArgLive args)
stgExprLive _ (StgOpApp _op args _ty) =
  mconcat (map stgArgLive args)
stgExprLive _ (StgLam bs e) =
  delDVarSetList (stgExprLive True e) (toList bs)
stgExprLive includeLHS (StgCase e b _at alts)
  | includeLHS = el `unionDVarSet` delDVarSet al b
  | otherwise  = delDVarSet al b
  where
    al = mconcat (map stgAltLive alts)
    el = stgExprLive True e
stgExprLive _ (StgLet _ b e) =
  delDVarSetList (stgBindRhsLive b `unionDVarSet` stgExprLive True e) (bindees b)
stgExprLive _ (StgLetNoEscape _ b e) =
  delDVarSetList (stgBindRhsLive b `unionDVarSet` stgExprLive True e) (bindees b)
stgExprLive _ (StgTick _ti e) =
  stgExprLive True e

stgAltLive :: StgAlt -> LiveVars
stgAltLive (_altCon, bs, e) =
  delDVarSetList (stgExprLive True e) bs

stgLetNoEscapeLive :: Bool -> StgBinding -> StgExpr -> LiveVars
stgLetNoEscapeLive _someBool _b _e = error "stgLetNoEscapeLive"

bindees :: StgBinding -> [Id]
bindees (StgNonRec b _e) = [b]
bindees (StgRec bs)      = map fst bs
