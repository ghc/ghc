{-# LANGUAGE TypeFamilies #-}

module T12386 where

class C a where
   type family F a t :: *

   type family T a :: *
   type T a = F a
