module T27845_Lib where

{-# INLINABLE [2] f #-}
f :: a -> Int -> [a]
f p x = lazyUse p x ++ lazyUse p (x + 1) ++ lazyUse p (x + 2)

{-# NOINLINE lazyUse #-}
lazyUse :: a -> Int -> [a]
lazyUse p x = p `seq` replicate x p

{-# RULES "lazyUse" [1] forall p x. lazyUse p x = [] #-}
