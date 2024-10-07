{-# LANGUAGE LambdaCase #-}

module GHC.StgToJS.Sinker.Collect
  ( collectArgsTop
  , collectArgs
  , selectUsedOnce
  )
  where

import GHC.Prelude
import GHC.Types.Unique.Set
import GHC.Stg.Syntax
import GHC.Types.Id
import GHC.Types.Unique

-- | fold over all id in StgArg used at the top level in an StgRhsCon
collectArgsTop :: CgStgBinding -> [Id]
collectArgsTop = \case
  StgNonRec _b r -> collectArgsTopRhs r
  StgRec bs      -> concatMap (collectArgsTopRhs . snd) bs
  where
    collectArgsTopRhs :: CgStgRhs -> [Id]
    collectArgsTopRhs = \case
      StgRhsCon _ccs _dc _mu _ticks args _typ -> concatMap collectArgsA args
      StgRhsClosure {}                        -> []

-- | fold over all Id in StgArg in the AST
collectArgs :: CgStgBinding -> [Id]
collectArgs = \case
  StgNonRec _b r -> collectArgsR r
  StgRec bs      -> concatMap (collectArgsR . snd) bs
  where
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

selectUsedOnce :: (Foldable t, Uniquable a) => t a -> UniqSet a
selectUsedOnce = fst . foldr g (emptyUniqSet, emptyUniqSet)
  where
    g i t@(once, mult)
      | i `elementOfUniqSet` mult = t
      | i `elementOfUniqSet` once
        = (delOneFromUniqSet once i, addOneToUniqSet mult i)
      | otherwise = (addOneToUniqSet once i, mult)
