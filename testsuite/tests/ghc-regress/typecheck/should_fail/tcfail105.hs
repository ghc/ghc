{-# LANGUAGE ExistentialQuantification #-}

-- Existential context should quantify over some new type variables
--
-- Jan07: this is now fine, because we've lifted the restrction
-- 	  that the context on a constructor should mention 
--	  existential type variables

module ShouldFail where

data S m t a = Ok a | Cont (M m t a)
data M m t a = Monad m => M { unM::(m (S m t a))}

