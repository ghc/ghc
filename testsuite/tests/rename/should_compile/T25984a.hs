{-# OPTIONS -Wdodgy-imports #-}

module T25984a where

import T25984a_helper hiding (H(A,B,C))

data T = A | B | C

t :: T
t = A
