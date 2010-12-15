{-# LANGUAGE DeriveFunctor #-}

module T4816 where

data Silly a = Sillly a

data Baz o = Baz {
     foo :: o,
     bar :: Silly ()
   } deriving (Functor)
