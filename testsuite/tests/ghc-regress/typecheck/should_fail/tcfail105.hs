{-# OPTIONS -fglasgow-exts #-}

-- Existential context should quantify over some new type variables

module ShouldFail where

data S m t a = Ok a | Cont (M m t a)
data M m t a = Monad m => M { unM::(m (S m t a))}

