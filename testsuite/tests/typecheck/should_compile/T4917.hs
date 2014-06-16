{-# LANGUAGE GADTs, ScopedTypeVariables, EmptyDataDecls, RankNTypes #-}

module T4917 where

-- only works on ghc6 but not on ghc7
type Const a b = a

newtype Fix f n = In { out :: f (Fix f) n }

mcata :: forall f a b .
         (forall x c . (forall d . x d -> Const b d) -> f x c -> Const b c)
      -> Fix f a -> Const b a
mcata f x = f {- x=(Fix f), c=a -} mcataf outx
  where
    outx :: f (Fix f) a
    outx = out x

    mcataf :: forall d. Fix f d -> Const b d
    mcataf y = mcata {- f=f, a=d, b=b0 -} f (y :: Fix f d)
    -- Const b d ~ Const b0 d
    -- Expected type of f :: forall x c. (forall d. x d -> Const b0 d) -> f x c -> Const b0 c
