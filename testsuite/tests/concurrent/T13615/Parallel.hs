{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, DefaultSignatures, TypeOperators, FlexibleContexts #-}

module Parallel
    (NFData, parMap, rdeepseq) where

import Control.Monad
import GHC.Exts
import Control.DeepSeq

infixl 0 `using`


type Strategy a = a -> Eval a

newtype Eval a = Eval (State# RealWorld -> (# State# RealWorld, a #))



instance Functor Eval where
  fmap = liftM

instance Applicative Eval where
  pure x = Eval $ \s -> (# s, x #)
  (<*>)  = ap

instance Monad Eval where
  return = pure
  Eval x >>= k = Eval $ \s -> case x s of
                                (# s', a #) -> case k a of
                                                      Eval f -> f s'

rpar :: Strategy a
rpar  x = Eval $ \s -> spark# x s

rparWith :: Strategy a -> Strategy a
rparWith s a = do l <- rpar r; return (case l of Lift x -> x)
  where r = case s a of
              Eval f -> case f realWorld# of
                          (# _, a' #) -> Lift a'

data Lift a = Lift a

using :: a -> Strategy a -> a
x `using` strat = runEval (strat x)


rdeepseq :: NFData a => Strategy a
rdeepseq x = do rseq (rnf x); return x

parList :: Strategy a -> Strategy [a]
parList strat = traverse (rparWith strat)

parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f = (`using` parList strat) . map f


runEval :: Eval a -> a
runEval (Eval x) = case x realWorld# of (# _, a #) -> a

rseq :: Strategy a
rseq x = Eval $ \s -> seq# x s
