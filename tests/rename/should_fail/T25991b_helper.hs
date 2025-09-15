{-# LANGUAGE TypeFamilies #-}

module T25991b_helper (C(..)) where

class C a b where
  type a # b
  (#) :: a -> b -> ()
