
{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}

module Main (main) where

import T2902_B_PairingSum

f :: Int -> PSum Int Int
f n = unions $ fmap g [1..n]
  where
    g m = unions $ fmap fromList
      [ zip [m..n] $ repeat 1
      , zip [m,2+m..n] $ repeat 2
      , zip [m,3+m..n] $ repeat 3
      ]

main ∷ IO ()
main = print $ take 20 $ toList $ f 20
