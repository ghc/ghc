{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TypeLambdaQuote (checkedColumns, labelledColumn, checkedCountCode) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import Language.Haskell.TH.Syntax (Code, Q)
import TypeLambda (Fields(..), Map)

$([d|
  type CheckedColumns e xs = Map (\cell -> Either e cell) xs
  |])

columnTypes :: $([t| CheckedColumns String '[Int, Bool] :~:
                     Map (\cell -> Either String cell) '[Int, Bool] |])
columnTypes = Refl

-- | An example row with a successfully decoded count and a missing flag.
checkedColumns :: Fields '[Either String Int, Either String Bool]
checkedColumns = case columnTypes of
  Refl -> $([| Field (Right 42) (Field (Left "missing flag") End)
                :: Fields (Map (\cell -> Either String cell) '[Int, Bool]) |])

$([d|
  -- | Attach a caller-supplied label to the Boolean column.
  labelledColumn :: forall (label :: Type). label -> Fields '[(label, Bool)]
  labelledColumn label = attach label
    where
      attach :: label -> Fields (Map (\cell -> (label, cell)) '[Bool])
      attach key = Field (key, True) End
  |])

-- | Quote the count with its checked cell type for a splice in another module.
checkedCountCode :: Code Q (Either String Int)
checkedCountCode = [|| Right 42 :: (\err cell -> Either err cell) String Int ||]
