{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TypeLambdaQuote (checkedColumns) where

import Data.Type.Equality ((:~:)(Refl))
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
