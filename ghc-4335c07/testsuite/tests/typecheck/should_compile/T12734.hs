{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeApplications          #-}

---------------  The original bug report --------------
--
-- See T12734a for a smaller version

module T12734 where

import Prelude
import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class


data A
data B
data Net
data Type

data Layer4 t l
data TermStore

-- Helpers: Stack

data Stack layers (t :: * -> *) where
    SLayer :: t l -> Stack ls t -> Stack (l ': ls) t
    SNull  :: Stack '[] t

instance ( Constructor m (t l)
         , Constructor m (Stack ls t)) => Constructor m (Stack (l ': ls) t)
instance Monad m                       => Constructor m (Stack '[]       t)


-- Helpers: Expr

newtype Expr  t layers    = Expr (TermStack t layers)
type TermStack t layers = Stack layers (Layer4 (Expr t layers))


-- Helpers: Funny typeclass

class Monad m => Constructor m t

instance ( Monad m, expr ~ Expr t layers, Constructor m (TermStack t layers)
         ) => Constructor m (Layer4 expr Type)


-- HERE IS A FUNNY BEHAVIOR: the commented line raises context reduction stack overflow
test_gr :: ( Constructor m (TermStack t layers), Inferable A layers m, Inferable B t m
            , bind ~ Expr t layers
--        ) => m (Expr t layers)
          ) => m bind
test_gr = undefined


-- Explicit information about a type which could be inferred

class Monad m => Inferable (cls :: *) (t :: k) m | cls m -> t

newtype KnownTypex (cls :: *) (t :: k) (m :: * -> *) (a :: *) = KnownTypex (IdentityT m a) deriving (Show, Functor, Monad, MonadIO, MonadFix, MonadTrans, Applicative, Alternative)

instance {-# OVERLAPPABLE #-} (t ~ t', Monad m)                              => Inferable cls t (KnownTypex cls t' m)
instance {-# OVERLAPPABLE #-} (Inferable cls t n, MonadTrans m, Monad (m n)) => Inferable cls t (m n)


runInferenceTx :: forall cls t m a. KnownTypex cls t m a -> m a
runInferenceTx = undefined



-- running it

test_ghc_err :: (MonadIO m, MonadFix m)
        => m (Expr Net '[Type])
test_ghc_err = runInferenceTx @B  @Net
             $ runInferenceTx @A @'[Type]
             $ (test_gr)
