{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module GHC.StgToJS.Sinker.StringsUnfloat
  ( unfloatStringLits
  )
  where

import GHC.Prelude
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Stg.Syntax
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Literal
import GHC.Utils.Misc (partitionWith)

import Data.ByteString qualified as BS
import Data.ByteString (ByteString)
import Data.Bifunctor (Bifunctor (..))

-- | We suppose that every string shorter than 80 symbols is safe for sink.
-- Sinker is working on per module. It means that ALL locally defined strings
-- in a module shorter 80 symbols will be unfloated back.
pattern STRING_LIT_MAX_LENGTH :: Int
pattern STRING_LIT_MAX_LENGTH = 80

unfloatStringLits
  :: UniqSet Name
  -> UniqFM Name ByteString
  -> [CgStgBinding]
  -> ([CgStgBinding], UniqSet Name)
unfloatStringLits usedOnceStringLits stringLits =
  unfloatStringLits' (selectStringLitsForUnfloat usedOnceStringLits stringLits)

-- | We are doing attempts to unfloat string literals back to
-- the call site. Further special JS optimizations
-- can generate more performant operations over them.
unfloatStringLits' :: UniqFM Name ByteString -> [CgStgBinding] -> ([CgStgBinding], UniqSet Name)
unfloatStringLits' stringLits allBindings = (binderWithoutChanges ++ binderWithUnfloatedStringLit, actuallyUsedStringLitNames)
  where
    (binderWithoutChanges, binderWithUnfloatedStringLitPairs) = partitionWith substituteStringLit allBindings

    binderWithUnfloatedStringLit = fst <$> binderWithUnfloatedStringLitPairs
    actuallyUsedStringLitNames = unionManyUniqSets (snd <$> binderWithUnfloatedStringLitPairs)

    substituteStringLit :: CgStgBinding -> Either CgStgBinding (CgStgBinding, UniqSet Name)
    substituteStringLit x@(StgRec bnds)
      | isEmptyUniqSet names = Left x
      | otherwise = Right (StgRec bnds', names)
      where
        (bnds', names) = extractNames id $ do
          (i, rhs) <- bnds
          pure $ case processStgRhs rhs of
            Nothing -> Left (i, rhs)
            Just (rhs', names) -> Right ((i, rhs'), names)
    substituteStringLit x@(StgNonRec binder rhs)
      = maybe (Left x)
        (\(body', names) -> Right (StgNonRec binder body', names))
        (processStgRhs rhs)

    processStgRhs :: CgStgRhs -> Maybe (CgStgRhs, UniqSet Name)
    processStgRhs (StgRhsCon ccs dataCon mu ticks args typ)
      | isEmptyUniqSet names = Nothing
      | otherwise = Just (StgRhsCon ccs dataCon mu ticks unified typ, names)
      where
        (unified, names) = substituteArgWithNames args
    processStgRhs (StgRhsClosure fvs ccs upd bndrs body typ)
      = (\(body', names) -> (StgRhsClosure fvs ccs upd bndrs body' typ, names)) <$>
        processStgExpr body

    -- Recursive expressions
    processStgExpr :: CgStgExpr -> Maybe (CgStgExpr, UniqSet Name)
    processStgExpr (StgLit _) = Nothing
    processStgExpr (StgTick _ _) = Nothing
    processStgExpr (StgLet n b e) =
      case (substituteStringLit b, processStgExpr e) of
        (Left _, Nothing) -> Nothing
        (Right (b', names), Nothing) -> Just (StgLet n b' e, names)
        (Left _, Just (e', names)) -> Just (StgLet n b e', names)
        (Right (b', names), Just (e', names')) -> Just (StgLet n b' e', names `unionUniqSets` names')
    processStgExpr (StgLetNoEscape n b e) =
      case (substituteStringLit b, processStgExpr e) of
        (Left _, Nothing) -> Nothing
        (Right (b', names), Nothing) -> Just (StgLetNoEscape n b' e, names)
        (Left _, Just (e', names)) -> Just (StgLetNoEscape n b e', names)
        (Right (b', names), Just (e', names')) -> Just (StgLetNoEscape n b' e', names `unionUniqSets` names')
    -- We should keep the order: See Note [Case expression invariants]
    processStgExpr (StgCase e bndr alt_type alts) =
      case (isEmptyUniqSet names, processStgExpr e) of
        (True, Nothing) -> Nothing
        (True, Just (e', names')) -> Just (StgCase e' bndr alt_type alts, names')
        (False, Nothing) -> Just (StgCase e bndr alt_type unified, names)
        (False, Just (e', names')) -> Just (StgCase e' bndr alt_type unified, names `unionUniqSets` names')
      where
        (unified, names) = extractNames splitAlts alts

        splitAlts :: CgStgAlt -> Either CgStgAlt (CgStgAlt, UniqSet Name)
        splitAlts alt@(GenStgAlt con bndrs rhs) =
          case processStgExpr rhs of
            Nothing -> Left alt
            Just (alt', names) -> Right (GenStgAlt con bndrs alt', names)

    -- No args
    processStgExpr (StgApp _ []) = Nothing
    processStgExpr (StgConApp _ _ [] _) = Nothing
    processStgExpr (StgOpApp _ [] _) = Nothing

    -- Main targets. Preserving the order of args is important
    processStgExpr (StgApp fn args@(_:_))
      | isEmptyUniqSet names = Nothing
      | otherwise = Just (StgApp fn unified, names)
      where
        (unified, names) = substituteArgWithNames args
    processStgExpr (StgConApp dc n args@(_:_) tys)
      | isEmptyUniqSet names = Nothing
      | otherwise = Just (StgConApp dc n unified tys, names)
      where
        (unified, names) = substituteArgWithNames args
    processStgExpr (StgOpApp op args@(_:_) tys)
      | isEmptyUniqSet names = Nothing
      | otherwise = Just (StgOpApp op unified tys, names)
      where
        (unified, names) = substituteArgWithNames args

    substituteArg :: StgArg -> Either StgArg (StgArg, Name)
    substituteArg a@(StgLitArg _) = Left a
    substituteArg a@(StgVarArg i) =
      let name = idName i
      in case lookupUFM stringLits name of
        Nothing -> Left a
        Just b -> Right (StgLitArg $ LitString b, name)

    substituteArgWithNames = extractNames (second (second unitUniqSet) . substituteArg)

    extractNames :: (a -> Either x (x, UniqSet Name)) -> [a] -> ([x], UniqSet Name)
    extractNames splitter target =
      let
        splitted = splitter <$> target
        combined = either (, emptyUniqSet) id <$> splitted
        unified = fst <$> combined
        names = unionManyUniqSets (snd <$> combined)
      in (unified, names)

selectStringLitsForUnfloat :: UniqSet Name -> UniqFM Name ByteString -> UniqFM Name ByteString
selectStringLitsForUnfloat usedOnceStringLits stringLits = alwaysUnfloat `plusUFM` usedOnceUnfloat
  where
    alwaysUnfloat = alwaysUnfloatStringLits stringLits
    usedOnceUnfloat = selectUsedOnceStringLits usedOnceStringLits stringLits

    alwaysUnfloatStringLits :: UniqFM Name ByteString -> UniqFM Name ByteString
    alwaysUnfloatStringLits = filterUFM $ \b -> BS.length b < STRING_LIT_MAX_LENGTH

    selectUsedOnceStringLits :: UniqSet Name -> UniqFM Name ByteString -> UniqFM Name ByteString
    selectUsedOnceStringLits usedOnceStringLits stringLits =
      stringLits `intersectUFM` getUniqSet usedOnceStringLits
