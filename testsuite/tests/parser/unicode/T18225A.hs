{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module T18225A where

(!) :: IO a -> b -> b
(!) _ = id

test1 :: Int
test1 = $⟦1⟧

test2 :: Int
test2 = ⟦2⟧!2
