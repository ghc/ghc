{-# LANGUAGE EmptyCase, EmptyDataDecls, TypeFamilies #-}

module T13468 where

import GHC.Generics

data Void

instance Generic Void where
   type Rep Void = V1
   from x = case x of
   to x = case x of
