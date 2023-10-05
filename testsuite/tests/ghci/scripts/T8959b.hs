{-# LANGUAGE UnicodeSyntax, Arrows, RankNTypes #-}
module T8959b where

foo :: Int -> Int
foo = ()

bar :: ()
bar = proc x -> do return -< x

baz = () :: (forall a. a -> a) -> a

