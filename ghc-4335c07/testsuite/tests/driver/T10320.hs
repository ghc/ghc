-- | Test file for issue #10320.

module Main (main) where

main :: IO ()
main = print $ map (+1) (map (+1) [1, 2, 3])

{-# RULES "map/map"    forall f g xs.  map f (map g xs) = map (f.g) xs #-}
