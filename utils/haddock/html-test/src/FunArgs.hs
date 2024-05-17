{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RankNTypes, DataKinds, TypeFamilies, TypeOperators #-}
module FunArgs where

f :: forall a. Ord a
  => Int          -- ^ First argument
  -> a            -- ^ Second argument
  -> Bool         -- ^ Third argument
  -> (a -> a)     -- ^ Fourth argument
  -> ()           -- ^ Result
f = undefined


g :: a -- ^ First argument
  -> b -- ^ Second argument
  -> c -- ^ Third argument
  -> d -- ^ Result
g = undefined


h :: forall a b c
  .  a -- ^ First argument
  -> b -- ^ Second argument
  -> c -- ^ Third argument
  -> forall d. d -- ^ Result
h _ _ _ = undefined


i :: forall a (b :: ()) d. (d ~ '())
  => forall c
  .  a b c d -- ^ abcd
  -> ()      -- ^ Result
i = undefined


j :: forall proxy (a :: ()) b
  .  proxy a -- ^ First argument
  -> b       -- ^ Result
j = undefined
