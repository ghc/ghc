{-# LANGUAGE GADTs #-}

module T9109 where

data G a where
  GBool :: G Bool

foo GBool = True
