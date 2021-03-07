{-# LANGUAGE Haskell2010 #-}
module Fixity where


(+++), (***), (///) :: a -> a -> a
(+++) = undefined
(***) = undefined
(///) = undefined


infix 6 +++
infixl 7 ***
infixr 8 ///
