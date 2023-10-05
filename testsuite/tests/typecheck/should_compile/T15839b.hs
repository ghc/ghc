{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-deriving-defaults #-}

module T15839a () where

class C a
newtype T a = MkT a deriving C
