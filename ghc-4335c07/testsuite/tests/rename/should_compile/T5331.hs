{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Foo where

data T a 

data S = forall a.S1

data W where
  W1 :: forall a. W

f :: forall a. Int
f = 3


