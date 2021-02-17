{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE PatternSynonyms #-}
module ShouldFail where

pattern Single{x} = [x]

-- Selector
selector :: Int
selector = x [5]

update :: [String]
update = ["String"] { x = "updated" }
