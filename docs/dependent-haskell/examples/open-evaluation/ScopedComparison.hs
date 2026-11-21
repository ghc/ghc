{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RoleAnnotations, ScopedTypeVariables #-}
module ScopedComparison (CompareZero(..), Compared(..), compareTagged) where

-- | Compare a value with the zero for its domain.
class CompareZero a where
    compareZero :: a -> Ordering

-- | Store the comparison result at its method index.
type role Compared nominal
data Compared (order :: Ordering) = Compared Ordering

tagOrdering :: foreach (order :: Ordering) -> Compared order
tagOrdering order = Compared @order order

-- | Tag the result of the caller's comparison method.
compareTagged :: forall a. CompareZero a => foreach (x :: a) -> Compared (compareZero x)
compareTagged x = tagOrdering (compareZero x)
