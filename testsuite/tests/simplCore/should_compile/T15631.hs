{-# Language PartialTypeSignatures, RankNTypes #-}

module Foo where

f xs = let ys = reverse xs
       in ys `seq`
          let w = length xs
          in w + length (reverse (case ys of { a:as -> as; [] -> [] }))



