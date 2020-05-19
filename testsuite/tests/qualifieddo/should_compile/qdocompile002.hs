{-# LANGUAGE QualifiedDo #-}

import Prelude as P hiding (fail)


-- Tests that fail is not required with irrefutable patterns
main =
  print $ P.do
    x <- [1, 2]
    (_, y) <- [(1, "a"), (2, "b")]
    P.return (x, y)
