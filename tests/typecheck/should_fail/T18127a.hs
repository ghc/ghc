{-# LANGUAGE RankNTypes #-}
module T18127a where

a :: (forall a. a) -> ()
a = undefined

b :: (Show a => a) -> ()
b = undefined

type C = forall a. a
c :: C -> ()
c = undefined

type D a = Show a => a
d :: D a -> ()
d = undefined
