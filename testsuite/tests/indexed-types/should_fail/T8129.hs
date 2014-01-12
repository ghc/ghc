{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module T8129 where

type family F (x :: *) :: *

class (y ~ F x) => C x y

z = () :: C x y => ()
