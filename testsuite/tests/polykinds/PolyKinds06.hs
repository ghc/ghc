{-# LANGUAGE DataKinds, KindSignatures  #-}
{-# LANGUAGE GADTs                      #-}

module PolyKinds06 where


data A = A1 | A2 (B 'B1)

data B :: A -> * where
  B1 :: B 'A1
