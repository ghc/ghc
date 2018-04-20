{-# LANGUAGE MultiParamTypeClasses #-}
module T14306 where

class (a `C` b) c

class (a `D` b) c d

class (a `E` b)

class a `F` b
