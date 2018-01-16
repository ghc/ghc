{-# LANGUAGE TypeFamilies, EmptyDataDecls, GADTs #-}

module T4484 where

type family F f :: *

data Id c = Id
type instance F (Id c) = c

data C :: * -> * where
  C :: f -> C (W (F f))

data W :: * -> *

fails :: C a -> C a
fails (C _)
  = -- We know (W (F f) ~ a)
    C Id  -- We need (a ~ W (F (Id beta)))
          --    ie   (a ~ W beta)
          -- Use the equality; we need
          --         (W (F f) ~ W beta)
          --    ie   (F f ~ beta)
          -- Solve with beta := f

works :: C (W a) -> C (W a)
works (C _)
  = -- We know (W (F f) ~ W a)
    C Id -- We need (W a ~ W (F (Id beta)))
         --      ie (W a ~ W beta)
         -- Solve with beta := a
