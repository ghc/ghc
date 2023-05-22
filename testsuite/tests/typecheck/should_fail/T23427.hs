module T23427 where

class C a where
    f :: a -> a

indent :: C a => a -> a
indent n = doText n
    where
      doText x = const (f x) doTail
      doTail _ = const n doText
