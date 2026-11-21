{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module NativeOpenSpanShapes (Shape, lazySpan, strictSpan) where

import Data.Kind (Type)

type family Shape where
    Shape = Type -> Type

lazySpan :: (a -> Bool) -> [a] -> ([a], [a])
lazySpan _ [] = ([], [])
lazySpan p input@(x : rest)
    | p x = let (ys, zs) = lazySpan p rest in (x : ys, zs)
    | otherwise = ([], input)

strictSpan :: (a -> Bool) -> [a] -> ([a], [a])
strictSpan _ [] = ([], [])
strictSpan p input@(x : rest)
    | p x = case strictSpan p rest of (ys, zs) -> (x : ys, zs)
    | otherwise = ([], input)
