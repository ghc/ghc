module FunArgs where

f :: forall a. Ord a
  => Int          -- ^ First argument
  -> a            -- ^ Second argument
  -> Bool         -- ^ Third argument
  -> (a -> a)     -- ^ Fourth argument
  -> ()           -- ^ Result
f = undefined
