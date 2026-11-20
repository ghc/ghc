{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
{-# LANGUAGE TypeFamilies #-}
module OrderingPayload (showComparison) where

import RuntimeOrdering (Compared(..), Witness(..), compareFrom)

-- | Select the payload type for each result of the comparison.
type family Select (o :: Ordering) where
    Select 'LT = Int
    Select 'EQ = Bool
    Select 'GT = Char

-- | Keep the selected value with its branch label and Show dictionary.
data Selected a where
    Selected :: Show a => String -> a -> Selected a

-- | Match the witness to refine the result of the payload family.
choose :: Witness o -> (Int, Bool, Char) -> Selected (Select o)
choose witness ~(n, b, c) = case witness of
    Less -> Selected "LT" n
    Equal -> Selected "EQ" b
    Greater -> Selected "GT" c

-- | Render the payload selected by the observed comparison.
showComparison :: Bool -> Bool -> (Int, Bool, Char) -> String
showComparison pivot x payload = case compareFrom pivot x of
    Compared _ _ result -> case choose result payload of
        Selected tag value -> tag ++ ":" ++ show value
