{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module GHC.StgToJS.Sinker (sinkPgm) where

import GHC.Prelude
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.Var.Set
import GHC.Stg.Syntax
import GHC.Types.Id
import GHC.Types.Name
import GHC.Unit.Module
import GHC.Types.Literal
import GHC.Data.Graph.Directed

import GHC.StgToJS.CoreUtils

import Data.Char
import Data.Either
import Data.List (partition)
import Data.Maybe


-- | Unfloat some top-level unexported things
--
-- GHC floats constants to the top level. This is fine in native code, but with JS
-- they occupy some global variable name. We can unfloat some unexported things:
--
-- - global constructors, as long as they're referenced only once by another global
--      constructor and are not in a recursive binding group
-- - literals (small literals may also be sunk if they are used more than once)
sinkPgm :: Module
        -> [CgStgTopBinding]
        -> (UniqFM Id CgStgExpr, [CgStgTopBinding])
sinkPgm m pgm = (sunk, map StgTopLifted pgm'' ++ stringLits)
  where
    selectLifted (StgTopLifted b) = Left b
    selectLifted x                = Right x
    (pgm', stringLits) = partitionEithers (map selectLifted pgm)
    (sunk, pgm'')      = sinkPgm' m pgm'

sinkPgm'
  :: Module
       -- ^ the module, since we treat definitions from the current module
       -- differently
  -> [CgStgBinding]
       -- ^ the bindings
  -> (UniqFM Id CgStgExpr, [CgStgBinding])
       -- ^ a map with sunken replacements for nodes, for where the replacement
       -- does not fit in the 'StgBinding' AST and the new bindings
sinkPgm' m pgm =
  let usedOnce = collectUsedOnce pgm
      sinkables = listToUFM $
          concatMap alwaysSinkable pgm ++
          filter ((`elementOfUniqSet` usedOnce) . fst) (concatMap (onceSinkable m) pgm)
      isSunkBind (StgNonRec b _e) | elemUFM b sinkables = True
      isSunkBind _                                      = False
  in (sinkables, filter (not . isSunkBind) $ topSortDecls m pgm)

-- | always sinkable, values that may be duplicated in the generated code (e.g.
-- small literals)
alwaysSinkable :: CgStgBinding -> [(Id, CgStgExpr)]
alwaysSinkable (StgRec {})       = []
alwaysSinkable (StgNonRec b rhs) = case rhs of
  StgRhsClosure _ _ _ _ e@(StgLit l) _
    | isSmallSinkableLit l
    , isLocal b
    -> [(b,e)]
  StgRhsCon _ccs dc cnum _ticks as@[StgLitArg l] _typ
    | isSmallSinkableLit l
    , isLocal b
    , isUnboxableCon dc
    -> [(b,StgConApp dc cnum as [])]
  _ -> []

isSmallSinkableLit :: Literal -> Bool
isSmallSinkableLit (LitChar c)     = ord c < 100000
isSmallSinkableLit (LitNumber _ i) = abs i < 100000
isSmallSinkableLit _               = False


-- | once sinkable: may be sunk, but duplication is not ok
onceSinkable :: Module -> CgStgBinding -> [(Id, CgStgExpr)]
onceSinkable _m (StgNonRec b rhs)
  | Just e <- getSinkable rhs
  , isLocal b = [(b,e)]
  where
    getSinkable = \case
      StgRhsCon _ccs dc cnum _ticks args _typ -> Just (StgConApp dc cnum args [])
      StgRhsClosure _ _ _ _ e@(StgLit{}) _typ -> Just e
      _                                       -> Nothing
onceSinkable _ _ = []

-- | collect all idents used only once in an argument at the top level
--   and never anywhere else
collectUsedOnce :: [CgStgBinding] -> IdSet
collectUsedOnce binds = intersectUniqSets (usedOnce args) (usedOnce top_args)
  where
    top_args = concatMap collectArgsTop binds
    args     = concatMap collectArgs    binds
    usedOnce = fst . foldr g (emptyUniqSet, emptyUniqSet)
    g i t@(once, mult)
      | i `elementOfUniqSet` mult = t
      | i `elementOfUniqSet` once
        = (delOneFromUniqSet once i, addOneToUniqSet mult i)
      | otherwise = (addOneToUniqSet once i, mult)

-- | fold over all id in StgArg used at the top level in an StgRhsCon
collectArgsTop :: CgStgBinding -> [Id]
collectArgsTop = \case
  StgNonRec _b r -> collectArgsTopRhs r
  StgRec bs      -> concatMap (collectArgsTopRhs . snd) bs

collectArgsTopRhs :: CgStgRhs -> [Id]
collectArgsTopRhs = \case
  StgRhsCon _ccs _dc _mu _ticks args _typ -> concatMap collectArgsA args
  StgRhsClosure {}                        -> []

-- | fold over all Id in StgArg in the AST
collectArgs :: CgStgBinding -> [Id]
collectArgs = \case
  StgNonRec _b r -> collectArgsR r
  StgRec bs      -> concatMap (collectArgsR . snd) bs

collectArgsR :: CgStgRhs -> [Id]
collectArgsR = \case
  StgRhsClosure _x0 _x1 _x2 _x3 e _typ     -> collectArgsE e
  StgRhsCon _ccs _con _mu _ticks args _typ -> concatMap collectArgsA args

collectArgsAlt :: CgStgAlt -> [Id]
collectArgsAlt alt = collectArgsE (alt_rhs alt)

collectArgsE :: CgStgExpr -> [Id]
collectArgsE = \case
  StgApp x args
    -> x : concatMap collectArgsA args
  StgConApp _con _mn args _ts
    -> concatMap collectArgsA args
  StgOpApp _x args _t
    -> concatMap collectArgsA args
  StgCase e _b _a alts
    -> collectArgsE e ++ concatMap collectArgsAlt alts
  StgLet _x b e
    -> collectArgs b ++ collectArgsE e
  StgLetNoEscape _x b e
    -> collectArgs b ++ collectArgsE e
  StgTick _i e
    -> collectArgsE e
  StgLit _
    -> []

collectArgsA :: StgArg -> [Id]
collectArgsA = \case
  StgVarArg i -> [i]
  StgLitArg _ -> []

isLocal :: Id -> Bool
isLocal i = isNothing (nameModule_maybe . idName $ i) && not (isExportedId i)

-- | since we have sequential initialization, topsort the non-recursive
-- constructor bindings
topSortDecls :: Module -> [CgStgBinding] -> [CgStgBinding]
topSortDecls _m binds = rest ++ nr'
  where
    (nr, rest) = partition isNonRec binds
    isNonRec StgNonRec{} = True
    isNonRec _           = False
    vs   = map getV nr
    keys = mkUniqSet (map node_key vs)
    getV e@(StgNonRec b _) = DigraphNode e b []
    getV _                 = error "topSortDecls: getV, unexpected binding"
    collectDeps (StgNonRec b (StgRhsCon _cc _dc _cnum _ticks args _typ)) =
      [ (i, b) | StgVarArg i <- args, i `elementOfUniqSet` keys ]
    collectDeps _ = []
    g = graphFromVerticesAndAdjacency vs (concatMap collectDeps nr)
    nr' | (not . null) [()| CyclicSCC _ <- stronglyConnCompG g]
            = error "topSortDecls: unexpected cycle"
        | otherwise = map node_payload (topologicalSortG g)
