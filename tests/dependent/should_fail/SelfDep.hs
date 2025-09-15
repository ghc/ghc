{-# LANGUAGE KindSignatures #-}

module SelfDep where

data T :: T -> *
