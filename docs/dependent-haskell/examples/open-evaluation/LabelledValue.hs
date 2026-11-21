{-# LANGUAGE GADTs #-}
module LabelledValue (LabelledValue(..), LabelledPair(..), StoredCheck(..)) where

-- | Store a number before choosing the type of the second field.
data LabelledValue a where
    LabelledValue :: !Int -> forall a. a -> LabelledValue a

-- | Choose each field's type after supplying the preceding value.
data LabelledPair a b where
    LabelledPair :: !Int -> forall a. a -> forall b. b -> LabelledPair a b

-- | Supply the equality dictionary after the label and before the payload.
data StoredCheck a where
    StoredCheck :: !Int -> forall a. Eq a => a -> StoredCheck a
