{-# LANGUAGE LinearTypes #-}
module LinearLet6 where

-- Should fail because the pattern is lazy and there is no clear interpretation
-- of linear lazy patterns.
f :: Maybe a %1 -> a
f x = let (Just y) = x in y

g :: Maybe a %1 -> a
g x = y
  where
    (Just y) = x

h :: Maybe a %1 -> a
h x = let %1 (Just y) = x in y
