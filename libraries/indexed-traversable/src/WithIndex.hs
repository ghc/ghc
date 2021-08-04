{-# LANGUAGE CPP                    #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy            #-}
{-# LANGUAGE DefaultSignatures      #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
module WithIndex where

import Prelude
       (Either (..), Functor (..), Int, Maybe (..), Monad (..), Num (..), error,
       flip, id, seq, snd, ($!), ($), (.), zip)

import Control.Applicative
       (Applicative (..), Const (..), ZipList (..), (<$>))
import Control.Applicative.Backwards (Backwards (..))
import Control.Monad.Trans.Identity  (IdentityT (..))
import Control.Monad.Trans.Reader    (ReaderT (..))
import Data.Array                    (Array)
import Data.Foldable                 (Foldable (..))
import Data.Functor.Compose          (Compose (..))
import Data.Functor.Constant         (Constant (..))
import Data.Functor.Identity         (Identity (..))
import Data.Functor.Product          (Product (..))
import Data.Functor.Reverse          (Reverse (..))
import Data.Functor.Sum              (Sum (..))
import Data.IntMap                   (IntMap)
import Data.Ix                       (Ix (..))
import Data.List.NonEmpty            (NonEmpty (..))
import Data.Map                      (Map)
import Data.Monoid                   (Dual (..), Endo (..), Monoid (..))
import Data.Proxy                    (Proxy (..))
import Data.Semigroup                (Semigroup (..))
import Data.Sequence                 (Seq)
import Data.Traversable              (Traversable (..))
import Data.Tree                     (Tree (..))
import Data.Void                     (Void)

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
       (K1 (..), Par1 (..), Rec1 (..), U1 (..), V1, (:*:) (..), (:+:) (..),
       (:.:) (..))
#else
import Generics.Deriving
       (K1 (..), Par1 (..), Rec1 (..), U1 (..), V1, (:*:) (..), (:+:) (..),
       (:.:) (..))
#endif

import qualified Data.Array    as Array
import qualified Data.IntMap   as IntMap
import qualified Data.Map      as Map
import qualified Data.Sequence as Seq

#ifdef MIN_VERSION_base_orphans
import Data.Orphans ()
#endif

#if __GLASGOW_HASKELL__ >=708
import Data.Coerce (Coercible, coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

-------------------------------------------------------------------------------
-- FunctorWithIndex
-------------------------------------------------------------------------------

-- | A 'Functor' with an additional index.
--
-- Instances must satisfy a modified form of the 'Functor' laws:
--
-- @
-- 'imap' f '.' 'imap' g ≡ 'imap' (\\i -> f i '.' g i)
-- 'imap' (\\_ a -> a) ≡ 'id'
-- @
class Functor f => FunctorWithIndex i f | f -> i where
  -- | Map with access to the index.
  imap :: (i -> a -> b) -> f a -> f b

#if __GLASGOW_HASKELL__ >= 704
  default imap :: TraversableWithIndex i f => (i -> a -> b) -> f a -> f b
  imap = imapDefault
  {-# INLINE imap #-}
#endif

imapDefault :: TraversableWithIndex i f => (i -> a -> b) -> f a -> f b
imapDefault f = runIdentity #. itraverse (\i a -> Identity (f i a))
{-# INLINE imapDefault #-}

-------------------------------------------------------------------------------
-- FoldableWithIndex
-------------------------------------------------------------------------------

-- | A container that supports folding with an additional index.
class Foldable f => FoldableWithIndex i f | f -> i where
  --
  -- | Fold a container by mapping value to an arbitrary 'Monoid' with access to the index @i@.
  --
  -- When you don't need access to the index then 'foldMap' is more flexible in what it accepts.
  --
  -- @
  -- 'foldMap' ≡ 'ifoldMap' '.' 'const'
  -- @
  ifoldMap :: Monoid m => (i -> a -> m) -> f a -> m

#if __GLASGOW_HASKELL__ >= 704
  default ifoldMap :: (TraversableWithIndex i f, Monoid m) => (i -> a -> m) -> f a -> m
  ifoldMap = ifoldMapDefault
  {-# INLINE ifoldMap #-}
#endif

  -- | A variant of 'ifoldMap' that is strict in the accumulator.
  ifoldMap' :: Monoid m => (i -> a -> m) -> f a -> m
  ifoldMap' f = ifoldl' (\i acc a -> mappend acc (f i a)) mempty
  {-# INLINE ifoldMap' #-}

  -- | Right-associative fold of an indexed container with access to the index @i@.
  --
  -- When you don't need access to the index then 'Data.Foldable.foldr' is more flexible in what it accepts.
  --
  -- @
  -- 'Data.Foldable.foldr' ≡ 'ifoldr' '.' 'const'
  -- @
  ifoldr   :: (i -> a -> b -> b) -> b -> f a -> b
  ifoldr f z t = appEndo (ifoldMap (\i -> Endo #. f i) t) z
  {-# INLINE ifoldr #-}

  -- | Left-associative fold of an indexed container with access to the index @i@.
  --
  -- When you don't need access to the index then 'Data.Foldable.foldl' is more flexible in what it accepts.
  --
  -- @
  -- 'Data.Foldable.foldl' ≡ 'ifoldl' '.' 'const'
  -- @
  ifoldl :: (i -> b -> a -> b) -> b -> f a -> b
  ifoldl f z t = appEndo (getDual (ifoldMap (\i -> Dual #. Endo #. flip (f i)) t)) z
  {-# INLINE ifoldl #-}

  -- | /Strictly/ fold right over the elements of a structure with access to the index @i@.
  --
  -- When you don't need access to the index then 'foldr'' is more flexible in what it accepts.
  --
  -- @
  -- 'foldr'' ≡ 'ifoldr'' '.' 'const'
  -- @
  ifoldr' :: (i -> a -> b -> b) -> b -> f a -> b
  ifoldr' f z0 xs = ifoldl f' id xs z0
    where f' i k x z = k $! f i x z
  {-# INLINE ifoldr' #-}

  -- | Fold over the elements of a structure with an index, associating to the left, but /strictly/.
  --
  -- When you don't need access to the index then 'Control.Lens.Fold.foldlOf'' is more flexible in what it accepts.
  --
  -- @
  -- 'Control.Lens.Fold.foldlOf'' l ≡ 'ifoldlOf'' l '.' 'const'
  -- @
  ifoldl' :: (i -> b -> a -> b) -> b -> f a -> b
  ifoldl' f z0 xs = ifoldr f' id xs z0
    where f' i x k z = k $! f i z x
  {-# INLINE ifoldl' #-}

ifoldMapDefault :: (TraversableWithIndex i f, Monoid m) => (i -> a -> m) -> f a -> m
ifoldMapDefault f = getConst #. itraverse (\i a -> Const (f i a))
{-# INLINE ifoldMapDefault #-}

-------------------------------------------------------------------------------
-- TraversableWithIndex
-------------------------------------------------------------------------------

-- | A 'Traversable' with an additional index.
--
-- An instance must satisfy a (modified) form of the 'Traversable' laws:
--
-- @
-- 'itraverse' ('const' 'Identity') ≡ 'Identity'
-- 'fmap' ('itraverse' f) '.' 'itraverse' g ≡ 'Data.Functor.Compose.getCompose' '.' 'itraverse' (\\i -> 'Data.Functor.Compose.Compose' '.' 'fmap' (f i) '.' g i)
-- @
class (FunctorWithIndex i t, FoldableWithIndex i t, Traversable t) => TraversableWithIndex i t | t -> i where
  -- | Traverse an indexed container.
  --
  -- @
  -- 'itraverse' ≡ 'itraverseOf' 'itraversed'
  -- @
  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)

#if __GLASGOW_HASKELL__ >= 704
  default itraverse :: (i ~ Int, Applicative f) => (i -> a -> f b) -> t a -> f (t b)
  itraverse f s = snd $ runIndexing (traverse (\a -> Indexing (\i -> i `seq` (i + 1, f i a))) s) 0
  {-# INLINE itraverse #-}
#endif

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance FunctorWithIndex r ((->) r) where
  imap f g x = f x (g x)
  {-# INLINE imap #-}

instance FunctorWithIndex () Maybe where
  imap f = fmap (f ())
  {-# INLINE imap #-}
instance FoldableWithIndex () Maybe where
  ifoldMap f = foldMap (f ())
  {-# INLINE ifoldMap #-}
instance TraversableWithIndex () Maybe where
  itraverse f = traverse (f ())
  {-# INLINE itraverse #-}

instance FunctorWithIndex Void Proxy where
  imap _ Proxy = Proxy
  {-# INLINE imap #-}

instance FoldableWithIndex Void Proxy where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void Proxy where
  itraverse _ _ = pure Proxy
  {-# INLINE itraverse #-}

instance FunctorWithIndex k ((,) k) where
  imap f (k,a) = (k, f k a)
  {-# INLINE imap #-}

instance FoldableWithIndex k ((,) k) where
  ifoldMap = uncurry'
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex k ((,) k) where
  itraverse f (k, a) = (,) k <$> f k a
  {-# INLINE itraverse #-}

-- | The position in the list is available as the index.
instance FunctorWithIndex Int [] where
  imap f = go 0 where
    go !_ []     = []
    go !n (x:xs) = f n x : go (n + 1) xs
  {-# INLINE imap #-}
instance FoldableWithIndex Int [] where
  ifoldMap = ifoldMapDefault
  {-# INLINE ifoldMap #-}
  ifoldr f z = go 0 where
    go !_ []     = z
    go !n (x:xs) = f n x (go (n + 1) xs)
  {-# INLINE ifoldr #-}
instance TraversableWithIndex Int [] where
  itraverse f = traverse (uncurry' f) . zip [0..]
  {-# INLINE itraverse #-}

-- TODO: we could experiment with streaming framework
-- imapListFB f xs = build (\c n -> ifoldr (\i a -> c (f i a)) n xs)

-- | Same instance as for @[]@.
instance FunctorWithIndex Int ZipList where
  imap f (ZipList xs) = ZipList (imap f xs)
  {-# INLINE imap #-}
instance FoldableWithIndex Int ZipList where
  ifoldMap f (ZipList xs) = ifoldMap f xs
  {-# INLINE ifoldMap #-}
instance TraversableWithIndex Int ZipList where
  itraverse f (ZipList xs) = ZipList <$> itraverse f xs
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- (former) semigroups
-------------------------------------------------------------------------------

instance FunctorWithIndex Int NonEmpty where
  imap = imapDefault
  {-# INLINE imap #-}
instance FoldableWithIndex Int NonEmpty where
  ifoldMap = ifoldMapDefault
  {-# INLINE ifoldMap #-}
instance TraversableWithIndex Int NonEmpty where
  itraverse f ~(a :| as) =
    (:|) <$> f 0 a <*> traverse (uncurry' f) (zip [1..] as)
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- Functors (formely) from transformers
-------------------------------------------------------------------------------

instance FunctorWithIndex () Identity where
  imap f (Identity a) = Identity (f () a)
  {-# INLINE imap #-}

instance FoldableWithIndex () Identity where
  ifoldMap f (Identity a) = f () a
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex () Identity where
  itraverse f (Identity a) = Identity <$> f () a
  {-# INLINE itraverse #-}

instance FunctorWithIndex Void (Const e) where
  imap _ (Const a) = Const a
  {-# INLINE imap #-}

instance FoldableWithIndex Void (Const e) where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void (Const e) where
  itraverse _ (Const a) = pure (Const a)
  {-# INLINE itraverse #-}

instance FunctorWithIndex Void (Constant e) where
  imap _ (Constant a) = Constant a
  {-# INLINE imap #-}

instance FoldableWithIndex Void (Constant e) where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void (Constant e) where
  itraverse _ (Constant a) = pure (Constant a)
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (i, j) (Compose f g) where
  imap f (Compose fg) = Compose $ imap (\k -> imap (f . (,) k)) fg
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (i, j) (Compose f g) where
  ifoldMap f (Compose fg) = ifoldMap (\k -> ifoldMap (f . (,) k)) fg
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (i, j) (Compose f g) where
  itraverse f (Compose fg) = Compose <$> itraverse (\k -> itraverse (f . (,) k)) fg
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (Either i j) (Sum f g) where
  imap q (InL fa) = InL (imap (q . Left)  fa)
  imap q (InR ga) = InR (imap (q . Right) ga)
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (Either i j) (Sum f g) where
  ifoldMap q (InL fa) = ifoldMap (q . Left)  fa
  ifoldMap q (InR ga) = ifoldMap (q . Right) ga
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (Either i j) (Sum f g) where
  itraverse q (InL fa) = InL <$> itraverse (q . Left)  fa
  itraverse q (InR ga) = InR <$> itraverse (q . Right) ga
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (Either i j) (Product f g) where
  imap f (Pair a b) = Pair (imap (f . Left) a) (imap (f . Right) b)
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (Either i j) (Product f g) where
  ifoldMap f (Pair a b) = ifoldMap (f . Left) a `mappend` ifoldMap (f . Right) b
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (Either i j) (Product f g) where
  itraverse f (Pair a b) = Pair <$> itraverse (f . Left) a <*> itraverse (f . Right) b
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- transformers
-------------------------------------------------------------------------------

instance FunctorWithIndex i m => FunctorWithIndex i (IdentityT m) where
  imap f (IdentityT m) = IdentityT $ imap f m
  {-# INLINE imap #-}

instance FoldableWithIndex i m => FoldableWithIndex i (IdentityT m) where
  ifoldMap f (IdentityT m) = ifoldMap f m
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i m => TraversableWithIndex i (IdentityT m) where
  itraverse f (IdentityT m) = IdentityT <$> itraverse f m
  {-# INLINE itraverse #-}

instance FunctorWithIndex i m => FunctorWithIndex (e, i) (ReaderT e m) where
  imap f (ReaderT m) = ReaderT $ \k -> imap (f . (,) k) (m k)
  {-# INLINE imap #-}

instance FunctorWithIndex i f => FunctorWithIndex i (Backwards f) where
  imap f  = Backwards . imap f . forwards
  {-# INLINE imap #-}

instance FoldableWithIndex i f => FoldableWithIndex i (Backwards f) where
  ifoldMap f = ifoldMap f . forwards
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i f => TraversableWithIndex i (Backwards f) where
  itraverse f = fmap Backwards . itraverse f . forwards
  {-# INLINE itraverse #-}

instance FunctorWithIndex i f => FunctorWithIndex i (Reverse f) where
  imap f = Reverse . imap f . getReverse
  {-# INLINE imap #-}

instance FoldableWithIndex i f => FoldableWithIndex i (Reverse f) where
  ifoldMap f = getDual . ifoldMap (\i -> Dual #. f i) . getReverse
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i f => TraversableWithIndex i (Reverse f) where
  itraverse f = fmap Reverse . forwards . itraverse (\i -> Backwards . f i) . getReverse
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- array
-------------------------------------------------------------------------------

instance Ix i => FunctorWithIndex i (Array i) where
  imap f arr = Array.listArray (Array.bounds arr) . fmap (uncurry' f) $ Array.assocs arr
  {-# INLINE imap #-}

instance Ix i => FoldableWithIndex i (Array i) where
  ifoldMap f = foldMap (uncurry' f) . Array.assocs
  {-# INLINE ifoldMap #-}

instance Ix i => TraversableWithIndex i (Array i) where
  itraverse f arr = Array.listArray (Array.bounds arr) <$> traverse (uncurry' f) (Array.assocs arr)
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance FunctorWithIndex [Int] Tree where
  imap f (Node a as) = Node (f [] a) $ imap (\i -> imap (f . (:) i)) as
  {-# INLINE imap #-}

instance FoldableWithIndex [Int] Tree where
  ifoldMap f (Node a as) = f [] a `mappend` ifoldMap (\i -> ifoldMap (f . (:) i)) as
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex [Int] Tree where
  itraverse f (Node a as) = Node <$> f [] a <*> itraverse (\i -> itraverse (f . (:) i)) as
  {-# INLINE itraverse #-}
--
-- | The position in the 'Seq' is available as the index.
instance FunctorWithIndex Int Seq where
  imap = Seq.mapWithIndex
  {-# INLINE imap #-}
instance FoldableWithIndex Int Seq where
#if MIN_VERSION_containers(0,5,8)
  ifoldMap = Seq.foldMapWithIndex
#else
  ifoldMap f = Data.Foldable.fold . Seq.mapWithIndex f
#endif
  {-# INLINE ifoldMap #-}
  ifoldr = Seq.foldrWithIndex
  {-# INLINE ifoldr #-}
  ifoldl f = Seq.foldlWithIndex (flip f)
  {-# INLINE ifoldl #-}
instance TraversableWithIndex Int Seq where
#if MIN_VERSION_containers(0,6,0)
  itraverse = Seq.traverseWithIndex
#else
  -- Much faster than Seq.traverseWithIndex for containers < 0.6.0, see
  -- https://github.com/haskell/containers/issues/603.
  itraverse f = sequenceA . Seq.mapWithIndex f
#endif
  {-# INLINE itraverse #-}

instance FunctorWithIndex Int IntMap where
  imap = IntMap.mapWithKey 
  {-# INLINE imap #-}

instance FoldableWithIndex Int IntMap where
#if MIN_VERSION_containers(0,5,4)
  ifoldMap = IntMap.foldMapWithKey
#else
  ifoldMap = ifoldMapDefault
#endif
  {-# INLINE ifoldMap #-}
#if MIN_VERSION_containers(0,5,0)
  ifoldr   = IntMap.foldrWithKey
  ifoldl'  = IntMap.foldlWithKey' . flip
  {-# INLINE ifoldr #-}
  {-# INLINE ifoldl' #-}
#endif

instance TraversableWithIndex Int IntMap where
#if MIN_VERSION_containers(0,5,0)
  itraverse = IntMap.traverseWithKey
#else
  itraverse f = sequenceA . IntMap.mapWithKey f
#endif
  {-# INLINE itraverse #-}

instance FunctorWithIndex k (Map k) where
  imap = Map.mapWithKey
  {-# INLINE imap #-}
  
instance FoldableWithIndex k (Map k) where
#if MIN_VERSION_containers(0,5,4)
  ifoldMap = Map.foldMapWithKey
#else
  ifoldMap = ifoldMapDefault
#endif
  {-# INLINE ifoldMap #-}
#if MIN_VERSION_containers(0,5,0)
  ifoldr   = Map.foldrWithKey
  ifoldl'  = Map.foldlWithKey' . flip
  {-# INLINE ifoldr #-}
  {-# INLINE ifoldl' #-}
#endif

instance TraversableWithIndex k (Map k) where
#if MIN_VERSION_containers(0,5,0)
  itraverse = Map.traverseWithKey
#else
  itraverse f = sequenceA . Map.mapWithKey f
#endif
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- GHC.Generics
-------------------------------------------------------------------------------

instance FunctorWithIndex Void V1 where
  imap _ v = v `seq` error "imap @V1"
  {-# INLINE imap #-}

instance FoldableWithIndex Void V1 where
  ifoldMap _ v = v `seq` error "ifoldMap @V1"

instance TraversableWithIndex Void V1 where
  itraverse _ v = v `seq` error "itraverse @V1"

instance FunctorWithIndex Void U1 where
  imap _ U1 = U1
  {-# INLINE imap #-}

instance FoldableWithIndex Void U1 where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void U1 where
  itraverse _ U1 = pure U1
  {-# INLINE itraverse #-}

instance FunctorWithIndex () Par1 where
  imap f = fmap (f ())
  {-# INLINE imap #-}

instance FoldableWithIndex () Par1 where
  ifoldMap f (Par1 a) = f () a
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex () Par1 where
  itraverse f (Par1 a) = Par1 <$> f () a
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (i, j) (f :.: g) where
  imap q (Comp1 fga) = Comp1 (imap (\k -> imap (q . (,) k)) fga)
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (i, j) (f :.: g) where
  ifoldMap q (Comp1 fga) = ifoldMap (\k -> ifoldMap (q . (,) k)) fga
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (i, j) (f :.: g) where
  itraverse q (Comp1 fga) = Comp1 <$> itraverse (\k -> itraverse (q . (,) k)) fga
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (Either i j) (f :*: g) where
  imap q (fa :*: ga) = imap (q . Left) fa :*: imap (q . Right) ga
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (Either i j) (f :*: g) where
  ifoldMap q (fa :*: ga) = ifoldMap (q . Left) fa `mappend` ifoldMap (q . Right) ga
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (Either i j) (f :*: g) where
  itraverse q (fa :*: ga) = (:*:) <$> itraverse (q . Left) fa <*> itraverse (q . Right) ga
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (Either i j) (f :+: g) where
  imap q (L1 fa) = L1 (imap (q . Left) fa)
  imap q (R1 ga) = R1 (imap (q . Right) ga)
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (Either i j) (f :+: g) where
  ifoldMap q (L1 fa) = ifoldMap (q . Left) fa
  ifoldMap q (R1 ga) = ifoldMap (q . Right) ga
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (Either i j) (f :+: g) where
  itraverse q (L1 fa) = L1 <$> itraverse (q . Left) fa
  itraverse q (R1 ga) = R1 <$> itraverse (q . Right) ga
  {-# INLINE itraverse #-}

instance FunctorWithIndex i f => FunctorWithIndex i (Rec1 f) where
  imap q (Rec1 f) = Rec1 (imap q f)
  {-# INLINE imap #-}

instance FoldableWithIndex i f => FoldableWithIndex i (Rec1 f) where
  ifoldMap q (Rec1 f) = ifoldMap q f
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i f => TraversableWithIndex i (Rec1 f) where
  itraverse q (Rec1 f) = Rec1 <$> itraverse q f
  {-# INLINE itraverse #-}

instance FunctorWithIndex Void (K1 i c) where
  imap _ (K1 c) = K1 c
  {-# INLINE imap #-}

instance FoldableWithIndex Void (K1 i c) where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void (K1 i c) where
  itraverse _ (K1 a) = pure (K1 a)
  {-# INLINE itraverse #-}

-------------------------------------------------------------------------------
-- Misc.
-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >=708
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
_ #. x = coerce x
#else
(#.) :: (b -> c) -> (a -> b) -> (a -> c)
_ #. x = unsafeCoerce x
#endif
infixr 9 #.
{-# INLINE (#.) #-}

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

------------------------------------------------------------------------------
-- Traversed
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.traverseOf_' and the like.
--
-- The argument 'a' of the result should not be used!
newtype Traversed a f = Traversed { getTraversed :: f a }

-- See 4.16 Changelog entry for the explanation of "why not Apply f =>"?
instance Applicative f => Semigroup (Traversed a f) where
  Traversed ma <> Traversed mb = Traversed (ma *> mb)
  {-# INLINE (<>) #-}

instance Applicative f => Monoid (Traversed a f) where
  mempty = Traversed (pure (error "Traversed: value used"))
  {-# INLINE mempty #-}
  Traversed ma `mappend` Traversed mb = Traversed (ma *> mb)
  {-# INLINE mappend #-}

------------------------------------------------------------------------------
-- Sequenced
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.mapM_' and the like.
--
-- The argument 'a' of the result should not be used!
--
-- See 4.16 Changelog entry for the explanation of "why not Apply f =>"?
newtype Sequenced a m = Sequenced { getSequenced :: m a }

instance Monad m => Semigroup (Sequenced a m) where
  Sequenced ma <> Sequenced mb = Sequenced (ma >> mb)
  {-# INLINE (<>) #-}

instance Monad m => Monoid (Sequenced a m) where
  mempty = Sequenced (return (error "Sequenced: value used"))
  {-# INLINE mempty #-}
  Sequenced ma `mappend` Sequenced mb = Sequenced (ma >> mb)
  {-# INLINE mappend #-}

------------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------------

-- | 'Applicative' composition of @'Control.Monad.Trans.State.Lazy.State' 'Int'@ with a 'Functor', used
-- by 'Control.Lens.Indexed.indexed'.
newtype Indexing f a = Indexing { runIndexing :: Int -> (Int, f a) }

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing mf <*> Indexing ma = Indexing $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}

-------------------------------------------------------------------------------
-- Strict curry
-------------------------------------------------------------------------------

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b
{-# INLINE uncurry' #-}
