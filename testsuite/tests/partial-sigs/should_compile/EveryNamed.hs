{-# LANGUAGE PartialTypeSignatures #-}
module EveryNamed where

every :: (_a -> Bool) -> [_a] -> Bool
every _ [] = True
every p (x:xs) = p x && every p xs
