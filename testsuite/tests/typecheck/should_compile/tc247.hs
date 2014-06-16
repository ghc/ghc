{-# LANGUAGE EmptyDataDecls, KindSignatures #-}

module ShouldCompile where

-- Various forms of empty data type declarations

data T1

data T2 where

data T3 :: * -> *

data T4 a :: * -> *

data T5 a :: * -> * where


