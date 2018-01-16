-- Test purpose:
--
-- Ensure the plural "s" in warnings is only shown if there are more than
-- one entries

{-# OPTIONS_GHC -Wredundant-constraints #-}
{-# OPTIONS_GHC -Wtype-defaults #-}

module PluralS () where



-- Defaulting type classes

defaultingNum = 123 `seq` ()

defaultingNumAndShow = show 123



-- Redundant constraints

redundantNum :: (Num a, Num a) => a
redundantNum = 123

redundantMultiple :: (Num a, Show a, Num a, Eq a, Eq a) => a
redundantMultiple = 123
