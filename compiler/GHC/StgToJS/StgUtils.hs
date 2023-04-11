{-# LANGUAGE LambdaCase #-}

module GHC.StgToJS.StgUtils
  ( bindingRefs
  , hasExport
  , collectTopIds
  , collectIds
  , removeTick
  , isUpdatableRhs
  , isInlineExpr
  , exprRefs
  -- * Live vars
  , LiveVars
  , liveVars
  , liveStatic
  , stgRhsLive
  , stgExprLive
  , stgTopBindLive
  , stgLetNoEscapeLive
  , stgLneLiveExpr
  , stgLneLive
  , stgLneLive'
  )
where

import GHC.Prelude

import GHC.Stg.Syntax
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Core.TyCon

import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Types.Unique
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.ForeignCall
import GHC.Types.TyThing
import GHC.Types.Name
import GHC.Types.Var.Set

import GHC.Builtin.Names
import GHC.Builtin.PrimOps (PrimOp(SeqOp), primOpIsReallyInline)
import GHC.Utils.Misc (seqList)
import GHC.Utils.Panic

import qualified Data.Foldable as F
import qualified Data.Set      as S
import qualified Data.List     as L
import Data.Set (Set)
import Data.Monoid

s :: a -> Set a
s = S.singleton

l :: (a -> Set Id) -> [a] -> Set Id
l = F.foldMap

-- | collect Ids that this binding refers to
--   (does not include the bindees themselves)
-- first argument is Id -> StgExpr map for unfloated arguments
bindingRefs :: UniqFM Id CgStgExpr -> CgStgBinding -> Set Id
bindingRefs u = \case
  StgNonRec _ rhs -> rhsRefs u rhs
  StgRec bs       -> l (rhsRefs u . snd) bs

rhsRefs :: UniqFM Id CgStgExpr -> CgStgRhs -> Set Id
rhsRefs u = \case
  StgRhsClosure _ _ _ _ body _       -> exprRefs u body
  StgRhsCon _ccs d _mu _ticks args _ -> l s [ i | AnId i <- dataConImplicitTyThings d] <> l (argRefs u) args

exprRefs :: UniqFM Id CgStgExpr -> CgStgExpr -> Set Id
exprRefs u = \case
  StgApp f args             -> s f <> l (argRefs u) args
  StgConApp d _n args _     -> l s [ i | AnId i <- dataConImplicitTyThings d] <> l (argRefs u) args
  StgOpApp _ args _         -> l (argRefs u) args
  StgLit {}                 -> mempty
  StgCase expr _ _ alts     -> exprRefs u expr <> mconcat (fmap (altRefs u) alts)
  StgLet _ bnd expr         -> bindingRefs u bnd <> exprRefs u expr
  StgLetNoEscape _ bnd expr -> bindingRefs u bnd <> exprRefs u expr
  StgTick _ expr            -> exprRefs u expr

altRefs :: UniqFM Id CgStgExpr -> CgStgAlt -> Set Id
altRefs u alt = exprRefs u (alt_rhs alt)

argRefs :: UniqFM Id CgStgExpr -> StgArg -> Set Id
argRefs u = \case
  StgVarArg id
    | Just e <- lookupUFM u id -> exprRefs u e
    | otherwise                -> s id
  _ -> mempty

hasExport :: CgStgBinding -> Bool
hasExport bnd =
  case bnd of
    StgNonRec b e -> isExportedBind b e
    StgRec bs     -> any (uncurry isExportedBind) bs
  where
    isExportedBind _i (StgRhsCon _cc con _ _ _ _) =
      getUnique con == staticPtrDataConKey
    isExportedBind _ _ = False

collectTopIds :: CgStgBinding -> [Id]
collectTopIds (StgNonRec b _) = [b]
collectTopIds (StgRec bs) = let xs = map (zapFragileIdInfo . fst) bs
                            in  seqList xs `seq` xs

collectIds :: UniqFM Id CgStgExpr -> CgStgBinding -> [Id]
collectIds unfloated b =
  let xs = map zapFragileIdInfo .
           filter acceptId $ S.toList (bindingRefs unfloated b)
  in  seqList xs `seq` xs
  where
    acceptId i = all ($ i) [not . isForbidden] -- fixme test this: [isExported[isGlobalId, not.isForbidden]
    -- the GHC.Prim module has no js source file
    isForbidden i
      | Just m <- nameModule_maybe (getName i) = m == gHC_PRIM
      | otherwise = False

removeTick :: CgStgExpr -> CgStgExpr
removeTick (StgTick _ e) = e
removeTick e             = e

-----------------------------------------------------
-- Live vars
--
-- TODO: should probably be moved into GHC.Stg.LiveVars

type LiveVars = DVarSet

liveStatic :: LiveVars -> LiveVars
liveStatic = filterDVarSet isGlobalId

liveVars :: LiveVars -> LiveVars
liveVars = filterDVarSet (not . isGlobalId)

stgTopBindLive :: CgStgTopBinding -> [(Id, LiveVars)]
stgTopBindLive = \case
  StgTopLifted b     -> stgBindLive b
  StgTopStringLit {} -> []

stgBindLive :: CgStgBinding -> [(Id, LiveVars)]
stgBindLive = \case
  StgNonRec b rhs -> [(b, stgRhsLive rhs)]
  StgRec bs       -> map (\(b,rhs) -> (b, stgRhsLive rhs)) bs

stgBindRhsLive :: CgStgBinding -> LiveVars
stgBindRhsLive b =
  let (bs, ls) = unzip (stgBindLive b)
  in  delDVarSetList (unionDVarSets ls) bs

stgRhsLive :: CgStgRhs -> LiveVars
stgRhsLive = \case
  StgRhsClosure _ _ _ args e _ -> delDVarSetList (stgExprLive True e) args
  StgRhsCon _ _ _ _ args _     -> unionDVarSets (map stgArgLive args)

stgArgLive :: StgArg -> LiveVars
stgArgLive = \case
  StgVarArg occ -> unitDVarSet occ
  StgLitArg {}  -> emptyDVarSet

stgExprLive :: Bool -> CgStgExpr -> LiveVars
stgExprLive includeLHS = \case
  StgApp occ args -> unionDVarSets (unitDVarSet occ : map stgArgLive args)
  StgLit {}       -> emptyDVarSet
  StgConApp _dc _n args _tys -> unionDVarSets (map stgArgLive args)
  StgOpApp _op args _ty      -> unionDVarSets (map stgArgLive args)
  StgCase e b _at alts
    | includeLHS -> el `unionDVarSet` delDVarSet al b
    | otherwise  -> delDVarSet al b
    where
      al = unionDVarSets (map stgAltLive alts)
      el = stgExprLive True e
  StgLet _ b e         -> delDVarSetList (stgBindRhsLive b `unionDVarSet` stgExprLive True e) (bindees b)
  StgLetNoEscape _ b e -> delDVarSetList (stgBindRhsLive b `unionDVarSet` stgExprLive True e) (bindees b)
  StgTick _ti e        -> stgExprLive True e

stgAltLive :: CgStgAlt -> LiveVars
stgAltLive alt =
  delDVarSetList (stgExprLive True (alt_rhs alt)) (alt_bndrs alt)

stgLetNoEscapeLive :: Bool -> StgBinding -> StgExpr -> LiveVars
stgLetNoEscapeLive _someBool _b _e = panic "stgLetNoEscapeLive"

bindees :: CgStgBinding -> [Id]
bindees = \case
  StgNonRec b _e -> [b]
  StgRec bs      -> map fst bs

isUpdatableRhs :: CgStgRhs -> Bool
isUpdatableRhs (StgRhsClosure _ _ u _ _ _) = isUpdatable u
isUpdatableRhs _                           = False

stgLneLive' :: CgStgBinding -> [Id]
stgLneLive' b = filter (`notElem` bindees b) (stgLneLive b)

stgLneLive :: CgStgBinding -> [Id]
stgLneLive (StgNonRec _b e) = stgLneLiveExpr e
stgLneLive (StgRec bs)      = L.nub $ concatMap (stgLneLiveExpr . snd) bs

stgLneLiveExpr :: CgStgRhs -> [Id]
stgLneLiveExpr rhs = dVarSetElems (liveVars $ stgRhsLive rhs)
-- stgLneLiveExpr (StgRhsClosure _ _ _ _ e) = dVarSetElems (liveVars (stgExprLive e))
-- stgLneLiveExpr StgRhsCon {}              = []

-- | returns True if the expression is definitely inline
isInlineExpr :: UniqSet Id -> CgStgExpr -> (UniqSet Id, Bool)
isInlineExpr v = \case
  StgApp i args
    -> (emptyUniqSet, isInlineApp v i args)
  StgLit{}
    -> (emptyUniqSet, True)
  StgConApp{}
    -> (emptyUniqSet, True)
  StgOpApp (StgFCallOp f _) _ _
    -> (emptyUniqSet, isInlineForeignCall f)
  StgOpApp (StgPrimOp SeqOp) [StgVarArg e] t
    -> (emptyUniqSet, e `elementOfUniqSet` v || isStrictType t)
  StgOpApp (StgPrimOp op) _ _
    -> (emptyUniqSet, primOpIsReallyInline op)
  StgOpApp (StgPrimCallOp _c) _ _
    -> (emptyUniqSet, True)
  StgCase e b _ alts
    ->let (_ve, ie)   = isInlineExpr v e
          v'          = addOneToUniqSet v b
          (vas, ias)  = unzip $ map (isInlineExpr v') (fmap alt_rhs alts)
          vr          = L.foldl1' intersectUniqSets vas
      in (vr, (ie || b `elementOfUniqSet` v) && and ias)
  StgLet _ b e
    -> isInlineExpr (inspectInlineBinding v b) e
  StgLetNoEscape _ _b e
    -> isInlineExpr v e
  StgTick  _ e
    -> isInlineExpr v e

inspectInlineBinding :: UniqSet Id -> CgStgBinding -> UniqSet Id
inspectInlineBinding v = \case
  StgNonRec i r -> inspectInlineRhs v i r
  StgRec bs     -> foldl' (\v' (i,r) -> inspectInlineRhs v' i r) v bs

inspectInlineRhs :: UniqSet Id -> Id -> CgStgRhs -> UniqSet Id
inspectInlineRhs v i = \case
  StgRhsCon{}                       -> addOneToUniqSet v i
  StgRhsClosure _ _ ReEntrant _ _ _ -> addOneToUniqSet v i
  _                                 -> v

isInlineForeignCall :: ForeignCall -> Bool
isInlineForeignCall (CCall (CCallSpec _ cconv safety)) =
  not (playInterruptible safety) &&
  not (cconv /= JavaScriptCallConv && playSafe safety)

isInlineApp :: UniqSet Id -> Id -> [StgArg] -> Bool
isInlineApp v i = \case
  _ | isJoinId i -> False
  [] -> isUnboxedTupleType (idType i) ||
                     isStrictType (idType i) ||
                     i `elementOfUniqSet` v

  [StgVarArg a]
    | DataConWrapId dc <- idDetails i
    , isNewTyCon (dataConTyCon dc)
    , isStrictType (idType a) || a `elementOfUniqSet` v || isStrictId a
    -> True
  _ -> False

