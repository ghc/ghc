{-# LANGUAGE QualifiedDo #-}

import Prelude as P hiding (fail)


-- Tests that fail is required with refutable patterns
main = do
  print $ P.do
    x <- [1, 2]
    (1, y) <- [(1, "a"), (2, "b")]
    P.return (x, y)
