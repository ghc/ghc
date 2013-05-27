{-# LANGUAGE TypeFamilies, PolyKinds #-}

module T7939 where

class Foo a where
   type Bar a


