{-# LANGUAGE MagicHash #-}
-- !! pattern-matching failure on functions that return Int#

--import PrelBase --ghc1.3
import GHC.Base

main = putStr (shows (I# (foo bar1 bar2)) "\n")
  where
    bar1 = Bar1 40 (39,38) resps
    bar2 = Bar1  2 ( 1, 0) resps
    resps = error "1.2 responses"

data Response = Response -- stub

data Bar
  = Bar1 Int (Int,Int) [Response]
  | Bar2 Int Int#
  | Bar3 Int

foo :: Bar -> Bar -> Int#

foo (Bar1 (I# i) _ _) (Bar1 (I# j) _ _) = i +# j
