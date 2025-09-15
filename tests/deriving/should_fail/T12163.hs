 {-# LANGUAGE DeriveFunctor #-}
 {-# LANGUAGE GADTs #-}

module T12163 where

data T a b where
     Mk :: Int -> b -> T Int b
     deriving (Functor)
