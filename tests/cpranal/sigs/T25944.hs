{-# LANGUAGE UndecidableInstances, LambdaCase #-}

-- | This file starts with a small reproducer for #25944 that is easy to debug
-- and then continues with a much larger MWE that is faithful to the original
-- issue.
module T25944 (foo, bar, popMinOneT, popMinOne) where

import Data.Functor.Identity ( Identity(..) )
import Data.Coerce

data ListCons a b = Nil | a :- !b
newtype Fix f = Fix (f (Fix f)) -- Rec

foo :: Fix (ListCons a) -> Fix (ListCons a) -> Fix (ListCons a)
foo a b = go a
  where
    -- The outer loop arranges it so that the base case `go as` of `go2` is
    -- bottom on the first iteration of the loop.
    go (Fix Nil) = Fix Nil
    go (Fix (a :- as)) = Fix (a :- go2 b)
      where
        go2 (Fix Nil) = go as
        go2 (Fix (b :- bs)) = Fix (b :- go2 bs)

bar :: Int -> (Fix (ListCons Int), Int)
bar n = (foo (Fix Nil) (Fix Nil), n) -- should still have CPR property

-- Now the actual reproducer from #25944:

newtype ListT m a = ListT { runListT :: m (ListCons a (ListT m a)) }

cons :: Applicative m => a -> ListT m a -> ListT m a
cons x xs = ListT (pure (x :- xs))

nil :: Applicative m => ListT m a
nil = ListT (pure Nil)

instance Functor m => Functor (ListT m) where
  fmap f (ListT m) = ListT (go <$> m)
     where
       go Nil = Nil
       go (a :- m) = f a :- (f <$> m)

foldListT :: ((ListCons a (ListT m a) -> c) -> m (ListCons a (ListT m a)) -> b)
          -> (a -> b -> c)
          -> c
          -> ListT m a -> b
foldListT r c n = r h . runListT
  where
    h Nil = n
    h (x :- ListT xs) = c x (r h xs)
{-# INLINE foldListT #-}

mapListT :: forall a m b. Monad m => (a -> ListT m b -> ListT m b) -> ListT m b -> ListT m a -> ListT m b
mapListT =
  foldListT
  ((coerce ::
 ((ListCons a (ListT m a) -> m (ListCons b (ListT m b))) -> m (ListCons a (ListT m a)) -> m (ListCons b (ListT m b))) ->
 ((ListCons a (ListT m a) -> ListT m b) -> m (ListCons a (ListT m a)) -> ListT m b))
  (=<<))
{-# INLINE mapListT #-}

instance Monad m => Applicative (ListT m) where
  pure x = cons x nil
  {-# INLINE pure #-}
  liftA2 f xs ys = mapListT (\x zs -> mapListT (cons . f x) zs ys) nil xs
  {-# INLINE liftA2 #-}

instance Monad m => Monad (ListT m) where
  xs >>= f = mapListT (flip (mapListT cons) . f) nil xs
  {-# INLINE (>>=) #-}

infixr 5 :<
data Node w a b = Leaf a | !w :< b
  deriving (Functor)

bimapNode f g (Leaf x) = Leaf (f x)
bimapNode f g (x :< xs) = x :< g xs

newtype HeapT w m a = HeapT { runHeapT :: ListT m (Node w a (HeapT w m a)) }

-- | The 'Heap' type, specialised to the 'Identity' monad.
type Heap w = HeapT w Identity

instance Functor m => Functor (HeapT w m) where
  fmap f = HeapT . fmap (bimapNode f (fmap f)) . runHeapT

instance Monad m => Applicative (HeapT w m) where
  pure = HeapT . pure . Leaf
  (<*>) = liftA2 id

instance Monad m => Monad (HeapT w m) where
  HeapT m >>= f = HeapT (m >>= g)
    where
      g (Leaf x) = runHeapT (f x)
      g (w :< xs) = pure (w :< (xs >>= f))

popMinOneT :: forall w m a. (Monoid w, Monad m) => HeapT w m a -> m (Maybe ((a, w), HeapT w m a))
popMinOneT = go mempty [] . runHeapT
  where
    go' :: w -> Maybe (w, HeapT w m a) -> m (Maybe ((a, w), HeapT w m a))
    go' a Nothing = pure Nothing
    go' a (Just (w, HeapT xs)) = go (a <> w) [] xs

    go :: w -> [(w, HeapT w m a)] -> ListT m (Node w a (HeapT w m a)) -> m (Maybe ((a, w), HeapT w m a))
    go w a (ListT xs) = xs >>= \case
      Nil -> go' w (undefined)
      Leaf x :- xs -> pure (Just ((x, w), undefined >> HeapT (foldl (\ys (yw,y) -> ListT (pure ((yw :< y) :- ys))) xs a)))
      (u :< x) :- xs -> go w ((u,x) : a) xs
{-# INLINE popMinOneT #-}

popMinOne :: Monoid w => Heap w a -> Maybe ((a, w), Heap w a)
popMinOne = runIdentity . popMinOneT
{-# INLINE popMinOne #-}
