module T11311 where

import Data.Kind

foo :: ()
foo = (id :: * -> *) undefined `seq` ()

main = print foo
