module Eta where

data T = MkT 
newtype Foo = Foo T

lift :: Foo -> T
lift (Foo x) = bof x	
  -- The point is that we expect 
  --   lift = bof |> co
  -- not
  --   lift = \fx -> bof (fx |> co)

bof :: T -> T
{-# NOINLINE bof #-}
bof MkT = MkT
