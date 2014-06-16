{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- Test for Trac #3955

module T3955 where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

class (Monad m) => MonadReader r m 
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap = liftM

instance Applicative (Reader r) where
    pure = return
    (<*>) = ap

instance Monad (Reader r) where
  (>>=)  = error "urk"
  return = error "urk"

instance MonadReader r (Reader r)

newtype T a x = T (Reader a x)
    deriving (Functor, Applicative, Monad, MonadReader a)

{-
[1 of 1] Compiling Main             ( bug.hs, interpreted )
mkUsageInfo: internal name? a{tv amy}
Ok, modules loaded: Main.
-}
