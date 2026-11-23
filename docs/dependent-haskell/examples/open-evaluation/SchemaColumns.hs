{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies, UndecidableInstances #-}
-- | Add an error schema to the open family defined by the row library.
module SchemaColumns (Callable, Checked) where

import TypeLambda (Columns, Fields, Map)

-- | Each field can contain the same error type.
data Checked e
type instance Columns (Checked e) fields = Map (\a -> Either e a) fields

-- | Store a function that wraps the values in a row.
data Callable e
type instance Columns (Callable e) fields =
  '[Fields fields -> Fields (Map (\a -> Either e a) fields)]
