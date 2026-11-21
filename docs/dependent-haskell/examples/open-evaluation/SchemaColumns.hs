{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies, UndecidableInstances #-}
-- | Add an error schema to the open family defined by the row library.
module SchemaColumns (Checked) where

import TypeLambda (Columns, Map)

-- | Each field can contain the same error type.
data Checked e
type instance Columns (Checked e) fields = Map (\a -> Either e a) fields
