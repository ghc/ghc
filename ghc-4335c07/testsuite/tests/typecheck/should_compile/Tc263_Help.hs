{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Tc263_Help where

class C a b where foo :: (a,b)

instance C [Int] b where foo = undefined
