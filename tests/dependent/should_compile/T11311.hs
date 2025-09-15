module T11311 where

import Data.Kind

foo :: ()
foo = (id :: Type -> Type) undefined `seq` ()

main = print foo
