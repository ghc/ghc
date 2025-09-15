{-# LANGUAGE GADTs, TypeFamilies #-}

module T8044 where

data X a where
  XInt :: X Int
  XBool :: X Bool
  XChar :: X Char

type family Frob a where
  Frob Int = Int
  Frob x   = Char

frob :: X a -> X (Frob a)
frob XInt = XInt
frob _    = XChar
