{-# LANGUAGE TypeFamilies #-}
module Test19821 where

type family F a b  = r | r -> a b where
  F Float  IO      = Float
  F Bool   IO      = Bool
  F a      IO      = IO a   -- (1)
  F Char   b       = b Int  -- (2)
