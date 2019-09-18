{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

safeLast :: [a] -> Maybe a
safeLast xs
  | []    <- reverse xs = Nothing
  | (x:_) <- reverse xs = Just x

safeLast2 :: [a] -> Maybe a
safeLast2 (reverse -> [])    = Nothing
safeLast2 (reverse -> (x:_)) = Just x
