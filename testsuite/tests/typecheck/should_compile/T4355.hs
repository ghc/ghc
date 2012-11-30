{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, RankNTypes, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, PatternGuards, DatatypeContexts #-}

module T4355 where

import Control.Arrow
import Control.Monad.Trans	-- From mtl
import Control.Monad.Reader	-- Ditto
import Data.Typeable
import Data.Maybe

class (Eq t, Typeable t) => Transformer t a | t -> a where
    transform :: (LayoutClass l a) => t -> l a ->
        (forall l'. (LayoutClass l' a) => l' a -> (l' a -> l a) -> b) -> b

class HList c a where
    find :: (Transformer t a) => c -> t -> Maybe Int

class Typeable a => Message a

data (LayoutClass l a) => EL l a = forall l'. (LayoutClass l' a) => EL (l' a) (l' a -> l a)

unEL :: (LayoutClass l a) => EL l a -> (forall l'. (LayoutClass l' a) => l' a -> b) -> b
unEL (EL x _) k = k x

transform' :: (Transformer t a, LayoutClass l a) => t -> EL l a -> EL l a
transform' t (EL l det) = transform t l (\l' det' -> EL l' (det . det'))

data Toggle a = forall t. (Transformer t a) => Toggle t
    deriving (Typeable)

instance (Typeable a) => Message (Toggle a)

data MultiToggle ts l a = MultiToggle{
    currLayout :: EL l a,
    currIndex :: Maybe Int,
    transformers :: ts
}

instance (Show ts, Show (l a), LayoutClass l a) => Show (MultiToggle ts l a) where

class Show (layout a) => LayoutClass layout a where
    handleMessage :: layout a -> SomeMessage -> IO (Maybe (layout a))

instance (Typeable a, Show ts, HList ts a, LayoutClass l a)
      => LayoutClass (MultiToggle ts l) a where
    handleMessage mt m
        | Just (Toggle t) <- fromMessage m
        , i@(Just _) <- find (transformers mt) t
            = case currLayout mt of
                EL l det -> do
                    return . Just $
                        mt {
                            currLayout = (if cur then id else transform' t) (EL (det l) id)
                        }
                    where cur = (i == currIndex mt)

data SomeMessage = forall a. Message a => SomeMessage a

fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m
