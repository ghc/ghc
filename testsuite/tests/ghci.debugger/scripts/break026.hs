module Test where

import Prelude hiding (foldl)

foldl f c xs = go c xs
  where go c []     = c
        go c (x:xs) = go (f c x) xs
