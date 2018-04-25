{-# LANGUAGE TypeOperators #-}

module A where

type a :+ b = (a,b)
infixr 1 :+


