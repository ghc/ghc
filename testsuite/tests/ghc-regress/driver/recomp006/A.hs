{-# LANGUAGE TypeOperators #-}
module A where

import B ((:-))

-- assumes :- is right-associative
f :: Int :- Int :- Int
f = (1,(2,3))

