{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QualifiedDo #-}

import Prelude as P


-- Tests that qualified dos show up in type-checking errors.
main = do
  print $ P.do
    x <- [1, 2]
    y@' ' <- [1, 2 :: Int]
    [1, 2]
    P.return y
