{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving #-}

module T15839a () where

class C a
newtype T a = MkT a deriving C
