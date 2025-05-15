{-# LANGUAGE TypeFamilies #-}

module T25899f_helper where

data T = X | Y | Z

class a # b where
  type F a b
  type G a b
  type H a b
