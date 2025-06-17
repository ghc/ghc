{-# LANGUAGE RankNTypes, ScopedTypeVariables, GHC2021 #-}

module T11339 where

import Control.Applicative ( Const(Const, getConst) )
import Data.Functor.Identity ( Identity(Identity) )

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

failing :: forall s t a b . Traversal s t a b -> Traversal s t a b -> Traversal s t a b
failing left right afb s = case pins t of
  [] -> right afb s
  _  -> t afb
  where
    t :: Applicative f => (a -> f b) -> f t
    -- Does not work because the MR applies to this binding group
    Bazaar { getBazaar = t } = left sell s

    sell :: a -> Bazaar a b b
    sell w   = Bazaar ($ w)

    pins :: ((a -> Const [Identity a] b) -> Const [Identity a] t) -> [Identity a]
    pins f   = getConst (f (\ra -> Const [Identity ra]))

newtype Bazaar a b t = Bazaar { getBazaar :: (forall f. Applicative f => (a -> f b) -> f t) }

instance Functor (Bazaar a b) where
  fmap f (Bazaar k) = Bazaar (fmap f . k)

instance Applicative (Bazaar a b) where
  pure a = Bazaar $ \_ -> pure a
  Bazaar mf <*> Bazaar ma = Bazaar $ \afb -> mf afb <*> ma afb
