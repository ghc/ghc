{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

-- #3590: a bug in typechecking of sections

module T3590 where

import Data.Kind (Type)

newtype ListT m a =
  ListT { runListT :: m (Maybe (a, ListT m a)) }

class Monad (ItemM l) => List l where
  type ItemM l :: Type -> Type
  joinL :: [ItemM l (l a) -> l a]

instance Monad m => List (ListT m) where
  type ItemM (ListT m) = m
  joinL = [ ListT . (>>= runListT)      -- Right section
          , ListT . (runListT <<=)      -- Left section
          ]

(<<=) :: Monad m => (a -> m b) -> m a -> m b
(<<=) k m = m >>= k

