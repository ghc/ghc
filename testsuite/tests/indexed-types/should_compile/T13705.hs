{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module T13705 where

data D x

type family F t = s | s -> t
type instance F (D t) = D (F t)

f :: F s -> ()
f _ = ()

g :: D (F t) -> ()
g x = f x
