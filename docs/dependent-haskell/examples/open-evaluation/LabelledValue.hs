{-# LANGUAGE GADTs, KindSignatures, RequiredTypeArguments #-}
module LabelledValue
    (LabelledValue(..), LabelledPair(..), StoredCheck(..), RankedPair(..), TypedNote(..), TypeTag(..)) where

import Data.Kind (Type)
import Type.Reflection (Typeable)

-- | Store a number before choosing the type of the second field.
data LabelledValue a where
    LabelledValue :: !Int -> forall a. a -> LabelledValue a

-- | Choose each field's type after supplying the preceding value.
data LabelledPair a b where
    LabelledPair :: !Int -> forall a. a -> forall b. b -> LabelledPair a b

-- | Supply the equality dictionary after the label and before the payload.
data StoredCheck a where
    StoredCheck :: !Int -> forall a. Eq a => a -> StoredCheck a

-- | Provide ordering before the field and the equality after it.
data RankedPair a where
    RankedPair :: Ord a => (a, a) -> (a ~ Int) => RankedPair a

-- | Supply the payload type after the number and before the payload.
data TypedNote a where
    TypedNote :: Int -> forall a -> a -> TypedNote a

-- | Store the dictionary for a type supplied after the number.
data TypeTag where
    TypeTag :: !Int -> forall (a :: Type) -> Typeable a => TypeTag
