{-# OPTIONS_GHC -fwrite-source-equations #-}
module NativeOpenSpanShapes (lazySpan, strictSpan) where

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
