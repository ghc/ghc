{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric      #-}
#ifndef __HADDOCK__
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE TypeOperators      #-}
#endif
#endif

#if MIN_VERSION_base(4,9,0)
#define LIFTED_FUNCTOR_CLASSES 1
#else
#if MIN_VERSION_transformers(0,5,0)
#define LIFTED_FUNCTOR_CLASSES 1
#else
#if MIN_VERSION_transformers_compat(0,5,0) && !MIN_VERSION_transformers(0,4,0)
#define LIFTED_FUNCTOR_CLASSES 1
#endif
#endif
#endif

-----------------------------------------------------------------------------
-- |
--
-- The strict variant of the standard Haskell pairs and the corresponding
-- variants of the functions from "Data.Tuple".
--
-- Note that unlike regular Haskell pairs, @(x :*: _|_) = (_|_ :*: y) = _|_@
--
-----------------------------------------------------------------------------

module Data.Strict.Tuple (
    Pair(..)
#ifndef __HADDOCK__
#ifdef __GLASGOW_HASKELL__
  , (:!:)
#endif
#endif
  , fst
  , snd
  , curry
  , uncurry
  , Data.Strict.Tuple.swap -- disambiguate
  , zip
  , unzip
) where

-- import parts explicitly, helps with compatibility
import           Prelude (Functor (..), Eq (..), Ord (..), Show (..), Read (..), (.), Bounded, map, ($)
                         , (&&), showParen, showString, readParen, lex, return)
import           Control.Applicative ((<$>), (<*>))
import           Data.Monoid (Monoid (..))
import           Data.Semigroup (Semigroup (..))
import           Data.Foldable (Foldable (..))
import           Data.Traversable (Traversable (..))

-- Lazy variants
import qualified Prelude             as L

import           Control.DeepSeq     (NFData (..))
import           Data.Bifoldable     (Bifoldable (..))
import           Data.Bifunctor      (Bifunctor (..))
import           Data.Binary         (Binary (..))
import           Data.Bitraversable  (Bitraversable (..))
import           Data.Hashable       (Hashable(..))
import           Data.Hashable.Lifted (Hashable1 (..), Hashable2 (..))
import           Data.Ix             (Ix (..))
import           GHC.Generics        (Generic)
import           Data.Data           (Data (..), Typeable)

#if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics        (Generic1)
#endif

#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData1 (..), NFData2 (..))
#endif

#ifdef MIN_VERSION_assoc
import           Data.Bifunctor.Assoc (Assoc (..))
import           Data.Bifunctor.Swap  (Swap (..))
#endif

#ifdef LIFTED_FUNCTOR_CLASSES
import Data.Functor.Classes
       (Eq1 (..), Eq2 (..), Ord1 (..), Ord2 (..), Read1 (..), Read2 (..),
       Show1 (..), Show2 (..))
#else
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Read1 (..), Show1 (..))
#endif

#if __HADDOCK__
import Data.Tuple ()
#endif

-- $setup
-- >>> import Prelude (Char, String)
-- >>> import Data.Functor.Classes (readsPrec2)

infix 2 :!:

-- | The type of strict pairs.
data Pair a b = !a :!: !b
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, Bounded, Ix
#if __GLASGOW_HASKELL__ >= 706
    , Generic1
#endif
    )

#ifndef __HADDOCK__
#ifdef __GLASGOW_HASKELL__
-- This gives a nicer syntax for the type but only works in GHC for now.
type (:!:) = Pair
#endif
#endif

toStrict :: (a, b) -> Pair a b
toStrict (a, b) = a :!: b

toLazy :: Pair a b -> (a, b)
toLazy (a :!: b) = (a, b)

-- | Extract the first component of a strict pair.
fst :: Pair a b -> a
fst (x :!: _) = x

-- | Extract the second component of a strict pair.
snd :: Pair a b -> b
snd (_ :!: y) = y

-- | Curry a function on strict pairs.
curry :: (Pair a b -> c) -> a -> b -> c
curry f x y = f (x :!: y)

-- | Convert a curried function to a function on strict pairs.
uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (x :!: y) = f x y

-- | Analogous to 'L.swap' from "Data.Tuple"
swap :: Pair a b -> Pair b a
swap (a :!: b) = b :!: a

-- | Zip for strict pairs (defined with zipWith).
zip :: [a] -> [b] -> [Pair a b]
zip x y = L.zipWith (:!:) x y

-- | Unzip for stict pairs into a (lazy) pair of lists.
unzip :: [Pair a b] -> ([a], [b])
unzip x = ( map fst x
          , map snd x
          )

-- Instances
------------

instance Functor (Pair e) where
    fmap f = toStrict . fmap f . toLazy

instance Foldable (Pair e) where
  foldMap f (_ :!: x) = f x

instance Traversable (Pair e) where
  traverse f (e :!: x) = (:!:) e <$> f x

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (x1 :!: y1) <> (x2 :!: y2) = (x1 <> x2) :!: (y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty                            = mempty :!: mempty
  (x1 :!: y1) `mappend` (x2 :!: y2) = (x1 `mappend` x2) :!: (y1 `mappend` y2)

-- deepseq
instance (NFData a, NFData b) => NFData (Pair a b) where
  rnf = rnf . toLazy

#if MIN_VERSION_deepseq(1,4,3)
instance (NFData a) => NFData1 (Pair a) where
  liftRnf rnfA = liftRnf rnfA . toLazy

instance NFData2 Pair where
  liftRnf2 rnfA rnfB = liftRnf2 rnfA rnfB . toLazy
#endif

-- binary
instance (Binary a, Binary b) => Binary (Pair a b) where
  put = put . toLazy
  get = toStrict <$> get

-- bifunctors
instance Bifunctor Pair where
  bimap f g (a :!: b) = f a :!: g b
  first f (a :!: b) = f a :!: b
  second g (a :!: b) = a :!: g b

instance Bifoldable Pair where
  bifold (a :!: b) = a `mappend` b
  bifoldMap f g (a :!: b) = f a `mappend` g b
  bifoldr f g c (a :!: b) = g b (f a c)
  bifoldl f g c (a :!: b) = g (f c a) b

instance Bitraversable Pair where
  bitraverse f g (a :!: b) = (:!:) <$> f a <*> g b

-- hashable
instance (Hashable a, Hashable b) => Hashable (Pair a b) where
  hashWithSalt salt = hashWithSalt salt . toLazy

instance (Hashable a) => Hashable1 (Pair a) where
  liftHashWithSalt hashA salt = liftHashWithSalt hashA salt . toLazy

instance Hashable2 Pair where
  liftHashWithSalt2 hashA hashB salt = liftHashWithSalt2 hashA hashB salt . toLazy

-- assoc
#ifdef MIN_VERSION_assoc
instance Assoc Pair where
    assoc ((a :!: b) :!: c) = (a :!: (b :!: c))
    unassoc (a :!: (b :!: c)) = ((a :!: b) :!: c)

instance Swap Pair where
    swap = Data.Strict.Tuple.swap
#endif

-- Data.Functor.Classes
#ifdef LIFTED_FUNCTOR_CLASSES
instance Eq2 Pair where
  liftEq2 f g (a :!: b) (a' :!: b')  = f a a' && g b b'

instance Eq a => Eq1 (Pair a) where
  liftEq = liftEq2 (==)

instance Ord2 Pair where
  liftCompare2 f g (a :!: b) (a' :!: b') = f a a' `mappend` g b b'

instance Ord a => Ord1 (Pair a) where
  liftCompare = liftCompare2 compare

instance Show a => Show1 (Pair a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 Pair where
  liftShowsPrec2 sa _ sb _ d (a :!: b) = showParen (d > 3)
    -- prints extra parens
    $ sa 3 a
    . showString " :!: "
    . sb 3 b

-- |
--
-- >>> readsPrec2 0 "'a' :!: ('b' :!: 'c')" :: [(Pair Char (Pair Char Char), String)]
-- [('a' :!: ('b' :!: 'c'),"")]
--
-- >>> readsPrec2 0 "('a' :!: 'b') :!: 'c'" :: [(Pair (Pair Char Char) Char, String)]
-- [(('a' :!: 'b') :!: 'c',"")]
--
instance Read2 Pair where
  liftReadsPrec2 ra _ rb _ d = readParen (d > 3) $ \s -> cons s where
    cons s0 = do
      (a,     s1) <- ra 3 s0
      (":!:", s2) <- lex s1
      (b,     s3) <- rb 3 s2
      return (a :!: b, s3)


instance Read a => Read1 (Pair a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList
#else
instance Eq a   => Eq1   (Pair a) where eq1        = (==)
instance Ord a  => Ord1  (Pair a) where compare1   = compare
instance Show a => Show1 (Pair a) where showsPrec1 = showsPrec
instance Read a => Read1 (Pair a) where readsPrec1 = readsPrec
#endif
