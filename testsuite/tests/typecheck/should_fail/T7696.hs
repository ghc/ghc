{-# LANGUAGE NoPolyKinds #-}

module T7696 where

f1 :: (m a, t m)
f1 = undefined

f2 :: ((), w ())
f2 = f1

{-
m :: * -> *
t :: (* -> *) -> *
a :: *
w :: * -> *

m a ~ ()
t m ~ w ()
-}
