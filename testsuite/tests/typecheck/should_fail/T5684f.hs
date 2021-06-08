{-# OPTIONS -XFunctionalDependencies -XUndecidableInstances -XFlexibleInstances #-}

module T5684 where

class B a b | a -> b where
  op :: a -> b -> ()

class A a | -> a

instance A b => B Bool b

flop6 =  [ op True undefined
         , op False False
         , op 'c' undefined
         ]
