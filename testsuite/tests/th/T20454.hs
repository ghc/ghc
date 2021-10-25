{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH

e1, e2 :: ExpQ

e1 = [|   -- Test the Template Haskell pretty-printing of rational literals
     [0.0, 123.0, -321.0, 9e3, 10000.0, -500000000.0, 345e67, -456e78,
      0.01, -0.002, 0.04e-56, -0.3e-65,
      0.33333333333333333333333333333, $(pure $ LitE $ RationalL $ 1/3)]
     |]

e2 = [|
      [[-4 .. -1],
        [-4, -3 .. -1],
        [-4, -3 ..],
        [-1 ..]]
      |]

main = runQ e1 >>= putStrLn . pprint
       >> runQ e2 >>= putStrLn . pprint
