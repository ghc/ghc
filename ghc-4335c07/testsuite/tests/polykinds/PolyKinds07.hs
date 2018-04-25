{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}

module PolyKinds07 where


data A = A1 | A2 (B 'B1)

data B a where
  B1 :: B 'A1

-- We correctly fail, but should probably provide a better error message
