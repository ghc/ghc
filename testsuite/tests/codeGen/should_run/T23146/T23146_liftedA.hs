{-# LANGUAGE DataKinds #-}

module T23146_liftedA where
  
data NP xs where
  UNil :: NP '[]
  (::*) :: x -> NP xs -> NP (x ': xs)

