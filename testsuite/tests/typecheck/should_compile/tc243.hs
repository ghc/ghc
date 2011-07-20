
{-# OPTIONS_GHC -Wall #-}

module Bug where

-- When we warn about this, we give a warning saying
--    Inferred type: (.+.) :: forall a. a
-- but we used to not print the parentheses.

(.+.) = undefined

