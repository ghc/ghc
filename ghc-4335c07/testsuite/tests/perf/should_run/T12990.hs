-- We used to inline constructor wrapper functions only when fully applied.
-- This led to unnecessary boxing when partially applying to unpacked fields.

module Main where
import Control.DeepSeq
import Data.Functor.Identity
import Control.Exception (evaluate)

data AList = Cons !Int !Int !Int !Int !Int !Int !Int !Int !Int AList | Nil

-- We need to write this instance manually because the Generic-derived
-- instance allocates a ton of intermediate junk, obscuring the interesting
-- differences.
instance NFData AList where
  rnf Nil = ()
  rnf (Cons _1 _2 _3 _4 _5 _6 _7 _8 _9 xs) = rnf xs

-- If GHC is allowed to specialize it to Identity, the partial application of
-- Cons will become a fully saturated one, defeating the test. So we NOINLINE
-- it.
buildalist :: Applicative f => Int -> f AList
buildalist n
  | n <= 0 = pure Nil
  | otherwise = Cons n (n+1) (n+2) (n+3) (n+4) (n+5) (n+6) (n+7) (n+8) <$>
                  buildalist (n - 1)
{-# NOINLINE buildalist #-}

main = evaluate . rnf . runIdentity $ buildalist 100000
