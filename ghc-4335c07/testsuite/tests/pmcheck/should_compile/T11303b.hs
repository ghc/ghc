{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Woverlapping-patterns #-}

module T11303b where

data family Letter a

data instance Letter a = A | B | C | D | E | F | G | H | I | J

f :: [Letter a] -> Int
f = foldl go 0
  where
    go n letter = n + n'
      where
        n' = case letter of
                  A -> 0
                  B -> 1
                  C -> 2
                  D -> 3
                  E -> 4
                  F -> 5
                  G -> 6
                  H -> 7
                  I -> 8
                  J -> 9
