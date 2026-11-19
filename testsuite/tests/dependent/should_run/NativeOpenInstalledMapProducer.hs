{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module NativeOpenInstalledMapProducer (mapper, example) where

{-# NOINLINE [0] mapper #-}
mapper :: (a -> b) -> [a] -> [b]
mapper _ [] = []
mapper f (x:xs) = f x : mapper f xs

#ifdef CHANGE_MAPPER
{-# RULES "drop mapper traversal" [~1] forall f xs. mapper f xs = [] #-}
#else
{-# RULES "mapper/id" [~1] forall xs. mapper (\x -> x) xs = xs #-}
#endif

example :: [Bool] -> [Bool]
example = mapper not
