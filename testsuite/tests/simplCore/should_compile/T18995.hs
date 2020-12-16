module T18995 where

import GHC.Magic ( noinline )

foo :: IO ()
foo = (noinline print) True
