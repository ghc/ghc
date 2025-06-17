{-# LANGUAGE NoMonomorphismRestriction, GHC2021 #-}
module T23427 where

class C a where
    f :: a -> a

indent :: C a => a -> a
indent n = doText n
    where
      doText x = const (f x) doTail
      doTail _ = const n doText

-- Test case from #20076
x :: Num a => a
(x, y) = (1.2, 3.4)
