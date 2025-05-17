{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module T13705 where

import Data.Kind (Type)

data D (x :: Type)

type family F t = s | s -> t
type instance F (D t) = D (F t)

f :: F s -> ()
f _ = ()

g :: D (F t) -> ()
g x = f x
