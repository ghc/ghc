{-# LANGUAGE QualifiedDo #-}

import Prelude as P hiding ((>>))


-- Tests that an out-of-scope (>>) is reported
main = do
  print $ P.do
    x <- [1, 2]
    y <- [1, 2]
    [1, 2]
    P.return (x, y)
