{-# LANGUAGE InstanceSigs #-}
module WildcardInInstanceSig where

instance Num Bool where
  negate :: _
  negate = undefined
