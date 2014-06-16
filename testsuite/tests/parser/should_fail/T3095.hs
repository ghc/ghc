{-# LANGUAGE KindSignatures #-}
-- Trac #3095
module T3095 where

class Bla (forall x . x :: *) where
