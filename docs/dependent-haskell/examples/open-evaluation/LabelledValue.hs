{-# LANGUAGE GADTs #-}
module LabelledValue (LabelledValue(..)) where

-- | Store a number before choosing the type of the second field.
data LabelledValue a where
    LabelledValue :: !Int -> forall a. a -> LabelledValue a
