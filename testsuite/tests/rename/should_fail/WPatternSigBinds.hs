{-# OPTIONS_GHC -Wpattern-signature-binds -Werror #-}
module WPatternSigBinds where

f (x :: a) = x

g (x :: a) (y :: b) = x

h (x :: a) (y :: b c d) = x

i :: forall f a . f a -> f a
i (x :: b c) = x
