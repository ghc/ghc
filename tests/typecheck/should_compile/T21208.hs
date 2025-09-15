{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module T21208 where

type family F a

inject :: a -> F a
inject = undefined

class C a where
  meth :: a -> ()

instance C a where
  meth _ = ()

g :: C a => a -> ()
g x = meth (inject x)
