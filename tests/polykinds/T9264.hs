{-# LANGUAGE PolyKinds, TypeFamilies, ScopedTypeVariables #-}
module T9264 where

class C (a :: k) where
   type F (a :: k)
   type F (a :: k) = Int
