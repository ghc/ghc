{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import TestUtils
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Either
import Data.Maybe
import Data.Bifunctor (first)
import GHC.Plugins (moduleNameString, nameStableString, nameOccName, occNameString, isDerivedOccName)
import GHC.Iface.Ext.Types
-- data EntityInfo
--   =
--   EntityVariable
boo = 1
--   | EntityFunction
foo :: a -> a
--   | EntityTypeVariable
foo a = a
--   | EntityTypeConstructor
data DataFoo
--   | EntityDataConstructor
   = DFoo
--   | EntityTypeClass
class ClassBoo a where
--   | EntityClassMethod
  cboo :: a
--   | EntityPatternSynonym
pattern PatternFoo = 1
--   | EntityTypeFamily
type family FamilyFoo
data family DataFamilyBar
--   | EntityTypeSynonym
type TypeSynonymFoo = Int
--   | EntityRecordField
data RecordFoo = RecordFoo { recordFoo :: Int }

points :: [(Int,Int)]
points = [(16,1), (18,9), (20,1), (22,6), (24,6), (26,7), (28,2), (30,9), (32,13), (33,13), (35,6), (37,30)]

getIdentifierEntityInfo :: HieFile -> Identifier -> S.Set EntityInfo
getIdentifierEntityInfo hf (Right ident) = M.findWithDefault S.empty ident (hie_entity_infos hf)
getIdentifierEntityInfo hf (Left _) = S.empty

isNotDerived :: (Identifier, a) -> Bool
isNotDerived ((Right name), _) = not $ isDerivedOccName (nameOccName name)
isNotDerived ((Left _), _) = True
main = do
  (df, hf) <- readTestHie "T24544.hie"
  let asts = fmap (fromMaybe (error "nothing") . selectPoint hf) points
      idents = concatMap (M.toList . sourcedNodeIdents . sourcedNodeInfo) asts
      names = map (\(x, _) -> (either moduleNameString  (occNameString . nameOccName) x, getIdentifierEntityInfo hf x)) $ filter isNotDerived idents
  mapM_ (print) names
