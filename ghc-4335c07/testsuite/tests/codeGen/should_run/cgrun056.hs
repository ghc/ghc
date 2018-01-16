-- Another test for the evaluated-ness of data2tag

module Main where

    data S e = A | B | C | D | E | F | G | H | I deriving (Eq)
    newtype R = T (S R) deriving (Eq)

    main = do { print (T A == T B) ; print (T I == T I) }
