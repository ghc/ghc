{-# LANGUAGE TypeFamilies #-}
module T7892 where

class C (f :: * -> *) where
   type F (f :: *) :: *


