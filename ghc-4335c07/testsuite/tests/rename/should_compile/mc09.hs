{-# LANGUAGE MonadComprehensions, TransformListComp #-}

-- Test performed for TransformListComp and should work for monad comp aswell:
-- GHC 7.0.1 failed because the renamer didn't attach
-- all the used variables to the TransformListComp constructor

module List where

intersectFront :: Ord a => [a] -> [a] -> [a]
intersectFront xs ys = [x | (x,y) <- zip xs ys, then takeWhile by x == y]
