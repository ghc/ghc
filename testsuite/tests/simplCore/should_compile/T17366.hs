module T17366 where
import Data.Functor.Identity
import T17366a

g :: Identity a -> a
g a = f a

h :: Tagged tag a -> a
h a = f a
