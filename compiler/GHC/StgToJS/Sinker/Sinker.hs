{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module GHC.StgToJS.Sinker.Sinker (sinkPgm) where

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
import GHC.StgToJS.Sinker.Collect
import GHC.StgToJS.Sinker.StringsUnfloat

import GHC.Utils.Misc (partitionWith)
import GHC.StgToJS.Utils

import Data.Char
import Data.List (partition)
import Data.Maybe
import Data.ByteString (ByteString)

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
sinkPgm m pgm
  = (sunk, map StgTopLifted pgm''' ++ stringLits)
  where
    selectLifted :: CgStgTopBinding -> Either CgStgBinding (Id, ByteString)
    selectLifted (StgTopLifted b)      = Left b
    selectLifted (StgTopStringLit i b) = Right (i, b)

    (pgm', allStringLits) = partitionWith selectLifted pgm
    usedOnceIds = selectUsedOnce $ concatMap collectArgs pgm'

    stringLitsUFM = listToUFM $ (\(i, b) -> (idName i, (i, b))) <$> allStringLits
    (pgm'', _actuallyUnfloatedStringLitNames) =
      unfloatStringLits
        (idName `mapUniqSet` usedOnceIds)
        (snd `mapUFM` stringLitsUFM)
        pgm'

    stringLits = uncurry StgTopStringLit <$> allStringLits

    (sunk, pgm''') = sinkPgm' m usedOnceIds pgm''

sinkPgm'
  :: Module
       -- ^ the module, since we treat definitions from the current module
       -- differently
  -> IdSet
       -- ^ the set of used once ids
  -> [CgStgBinding]
       -- ^ the bindings
  -> (UniqFM Id CgStgExpr, [CgStgBinding])
       -- ^ a map with sunken replacements for nodes, for where the replacement
       -- does not fit in the 'StgBinding' AST and the new bindings
sinkPgm' m usedOnceIds pgm =
  let usedOnce = collectTopLevelUsedOnce usedOnceIds pgm
      sinkables = listToUFM $
          concatMap alwaysSinkable pgm ++
          concatMap (filter ((`elementOfUniqSet` usedOnce) . fst) . onceSinkable m) pgm
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
collectTopLevelUsedOnce :: IdSet -> [CgStgBinding] -> IdSet
collectTopLevelUsedOnce usedOnceIds binds = intersectUniqSets usedOnceIds (selectUsedOnce top_args)
  where
    top_args = concatMap collectArgsTop binds

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
