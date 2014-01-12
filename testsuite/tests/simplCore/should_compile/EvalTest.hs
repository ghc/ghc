-- There was a bug in 6.12 that meant that the binding
-- for 'rght' was initially determined (correctly) to be
-- strictly demanded, but the FloatOut pass made it lazy
--
-- The test compiles the program and greps for the 
-- binding of 'rght' to check that it is marked strict
-- somethign like this:
--         rght [Dmd=Just S] :: EvalTest.AList a

module EvalTest where

import GHC.Conc

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

data Eval a = Done a

instance Functor Eval where
    fmap = liftM

instance Applicative Eval where
    pure = return
    (<*>) = ap

instance Monad Eval where
  return x = Done x
  Done x >>= k = k x   -- Note: pattern 'Done x' makes '>>=' strict

rpar :: a -> Eval a
rpar x = x `par` return x

rseq :: a -> Eval a
rseq x = x `pseq` return x

runEval :: Eval a -> a
runEval (Done x) = x

data AList a = ANil | ASing a | Append (AList a) (AList a) | AList [a]

append ANil r = r
append l ANil = l -- **
append l r    = Append l r

parListTreeLike :: Integer -> Integer -> (Integer -> a) -> AList a
parListTreeLike min max fn
 | max - min <= threshold = ASing (fn max)
 | otherwise  =
      runEval $ do
        rpar rght
        rseq left
        return (left `append` rght)
    where
      mid  = min + ((max - min) `quot` 2)
      left = parListTreeLike min mid fn
      rght = parListTreeLike (mid+1) max fn

threshold = 1
