{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module RuntimeOrdering (Witness(..), Compared(..), decideOrdering, compareFrom) where

-- | Record the observed result of the comparison.
data Witness (o :: Ordering) where
    Less :: Witness 'LT
    Equal :: Witness 'EQ
    Greater :: Witness 'GT

-- | Observe the result of applying this comparator to this input.
decideOrdering :: foreach (p :: Bool -> Ordering)
               -> foreach (x :: Bool) -> Witness (p x)
decideOrdering p x = case p x of
    LT -> Less
    EQ -> Equal
    GT -> Greater

-- | Store the comparator, its input, and the witness for their application.
data Compared where
    Compared :: foreach (p :: Bool -> Ordering) -> foreach (x :: Bool)
             -> Witness (p x) -> Compared

-- | Capture a pivot at runtime with the ordinary Boolean comparison.
compareFrom :: Bool -> Bool -> Compared
compareFrom pivot x =
    let p :: Bool -> Ordering
        p value = compare pivot value
    in Compared p x (decideOrdering p x)
