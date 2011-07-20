{-# LANGUAGE TypeFamilies #-}

module Simple8 where

type family F a

-- Manuel says that duplicate instances are ok. This gives a strange error but
-- works if one of the duplicates is removed.

type instance F () = ()
type instance F () = ()

foo :: F () -> ()
foo x = x

