{-# LANGUAGE LinearTypes, NoModifiers #-}

module LinearLet10 where

-- Test for well-kindedness of multiplicity annotations
f :: a -> a
f x = let %Int y = x in y -- MODS_TODO why isn't this allowed with NoModifiers?
