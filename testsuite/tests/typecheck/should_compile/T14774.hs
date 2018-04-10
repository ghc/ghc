{-# LANGUAGE UndecidableSuperClasses #-}

module T14774 where

class C a => D a where
  cop :: a -> Bool

class D a => C a where
  dop :: a -> a

f :: C a => Int -> a -> Bool
f 0 x = cop x
f n x = f (n-1) x
