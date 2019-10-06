{-# Language PartialTypeSignatures, RankNTypes #-}

module Foo where

import Prelude hiding (reverse)

f xs = let ys = reverse xs
       in ys `seq`
          let w = length xs
          in w + length (reverse (case ys of { a:as -> as; [] -> [] }))

reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)

