module NativeClosedCallbackLib (each, eachBool, setTrue, flipBool) where

-- | Apply a function to every element.
each :: (a -> b) -> [a] -> [b]
each _ [] = []
each f (x:xs) = f x : each f xs

-- | Use a fixed callback type for the argument-kind check.
eachBool :: (Bool -> Bool) -> [Bool] -> [Bool]
eachBool = each

-- | Ignore the input.
setTrue :: Bool -> Bool
setTrue _ = True

-- | Complement a Boolean.
flipBool :: Bool -> Bool
flipBool True = False
flipBool False = True
