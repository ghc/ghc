module RttiSharedTyVar where

f :: a -> a -> ()
f x y = x `seq` y `seq` ()
