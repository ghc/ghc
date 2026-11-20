{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module OrderingPayload (showComparison) where

import Data.Type.Equality ((:~:)(Refl))
import RuntimeOrdering (Compared(..), Witness(..), compareFrom)

-- | Select the payload type for each result of the comparison.
type family Select (o :: Ordering) where
    Select 'LT = Int
    Select 'EQ = Bool
    Select 'GT = Char

data Selected a where
    Selected :: Show a => String -> a -> Selected a

congruence :: q :~: r -> Select q :~: Select r
congruence Refl = Refl

transport :: a :~: b -> Selected b -> Selected a
transport Refl x = x

choose :: forall o. Witness o -> (Int, Bool, Char) -> Selected (Select o)
choose witness ~(n, b, c) = case witness of
    Less -> transport (congruence (Refl :: o :~: 'LT)) (Selected "LT" n)
    Equal -> transport (congruence (Refl :: o :~: 'EQ)) (Selected "EQ" b)
    Greater -> transport (congruence (Refl :: o :~: 'GT)) (Selected "GT" c)

-- | Render the payload selected by the observed comparison.
showComparison :: Bool -> Bool -> (Int, Bool, Char) -> String
showComparison pivot x payload = case compareFrom pivot x of
    Compared _ _ result -> case choose result payload of
        Selected tag value -> tag ++ ":" ++ show value
