{-# LANGUAGE MultiParamTypeClasses, ExplicitForAll #-}

module ShouldFail where

-- !!! For-all with parens

-- This one crashed ghc-4.04proto; the parens after the for-all fooled it

class Monad m => StateMonad s m where
   getState :: m s

setState0 :: forall b. (StateMonad (a,b) m => m a)
setState0 = getState >>= \ (l,_r) -> return l


