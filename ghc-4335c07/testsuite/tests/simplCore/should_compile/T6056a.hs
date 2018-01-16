module T6056a where

smallerAndRest :: Ord a => a -> [a] -> (Maybe a, [a])
smallerAndRest x [] = (Nothing, [])
smallerAndRest x (y:ys) | y < x = (Just y, ys)
                        | otherwise = smallerAndRest x ys

{-# INLINABLE smallerAndRest #-}
