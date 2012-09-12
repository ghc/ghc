{-# LANGUAGE PolyKinds, TypeFamilies #-}

module T7073 where

class Foo a where
   type Bar a
   type Bar a = Int

