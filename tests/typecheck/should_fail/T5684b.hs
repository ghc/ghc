{-# OPTIONS -XFunctionalDependencies -XUndecidableInstances -XFlexibleInstances #-}

module T5684b where

class B a b | a -> b where
  op :: a -> b -> ()

class A a | -> a

instance A b => B Bool b

flop2 =  [ op False False
         , op True undefined
         , op 'c' undefined
         ]
