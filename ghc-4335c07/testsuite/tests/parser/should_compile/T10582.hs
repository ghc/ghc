{-# LANGUAGE Arrows #-}

module T10582 where

(|:) :: Int -> Int -> Int
(|:) = (+)
