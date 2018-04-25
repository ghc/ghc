{-# LANGUAGE MultiParamTypeClasses, StandaloneDeriving #-}
module T7959 where

class A
deriving instance A
data B deriving A
