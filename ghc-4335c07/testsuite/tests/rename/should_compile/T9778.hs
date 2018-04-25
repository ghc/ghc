{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module T9778 where

data T = A | B

data G a where
  C :: G A
