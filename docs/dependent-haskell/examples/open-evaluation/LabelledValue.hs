{-# LANGUAGE GADTs #-}
module LabelledValue (LabelledValue(..), LabelledPair(..)) where

-- | Store a number before choosing the type of the second field.
data LabelledValue a where
    LabelledValue :: !Int -> forall a. a -> LabelledValue a

-- | Choose each field's type after supplying the preceding value.
data LabelledPair a b where
    LabelledPair :: !Int -> forall a. a -> forall b. b -> LabelledPair a b
