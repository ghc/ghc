{-# LANGUAGE TypeFamilies #-}
module T4093a where

type family Foo x
type instance Foo () = Maybe ()

hang :: (Foo e ~ Maybe e) => Foo e
hang = Just ()
