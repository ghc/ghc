{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric      #-}

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
-- The strict variant of the standard Haskell 'L.Either' type and the
-- corresponding variants of the functions from "Data.Either".
--
-- Note that the strict 'Either' type is not an applicative functor, and
-- therefore also no monad. The reasons are the same as the ones for the
-- strict @Maybe@ type, which are explained in "Data.Maybe.Strict".
--
-----------------------------------------------------------------------------

module Data.Strict.Either (
    Either(..)
  , either
  , isLeft, isRight
  , fromLeft, fromRight
  , lefts, rights
  , partitionEithers
) where

-- import parts explicitly, helps with compatibility
import           Prelude ( Functor (..), Eq (..), Ord (..), Show (..), Read (..), Bool (..), (.), ($)
                         , error, Ordering (..), showParen, showString, lex, return, readParen)
import           Control.Applicative (pure, (<$>))
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

-- | The strict choice type.
data Either a b = Left !a | Right !b
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic
#if __GLASGOW_HASKELL__ >= 706
    , Generic1
#endif
    )

toStrict :: L.Either a b -> Either a b
toStrict (L.Left x)  = Left x
toStrict (L.Right y) = Right y

toLazy :: Either a b -> L.Either a b
toLazy (Left x)  = L.Left x
toLazy (Right y) = L.Right y

-- | Case analysis: if the value is @'Left' a@, apply the first function to @a@;
-- if it is @'Right' b@, apply the second function to @b@.
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left  x) = f x
either _ g (Right y) = g y

-- | Yields 'True' iff the argument is of the form @Left _@.
--
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- | Yields 'True' iff the argument is of the form @Right _@.
--
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- | Extracts the element out of a 'Left' and throws an error if the argument
-- is a 'Right'.
fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _        = error "Data.Strict.Either.fromLeft: Right"

-- | Extracts the element out of a 'Right' and throws an error if the argument
-- is a 'Left'.
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "Data.Strict.Either.fromRight: Left"

-- | Analogous to 'L.lefts' in "Data.Either".
lefts   :: [Either a b] -> [a]
lefts x = [a | Left a <- x]

-- | Analogous to 'L.rights' in "Data.Either".
rights   :: [Either a b] -> [b]
rights x = [a | Right a <- x]

-- | Analogous to 'L.partitionEithers' in "Data.Either".
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers =
    L.foldr (either left right) ([],[])
  where
    left  a ~(l, r) = (a:l, r)
    right a ~(l, r) = (l, a:r)

-- Instances
------------

instance Functor (Either a) where
  fmap _ (Left  x) = Left x
  fmap f (Right y) = Right (f y)

instance Foldable (Either e) where
  foldr _ y (Left _)  = y
  foldr f y (Right x) = f x y

  foldl _ y (Left _)  = y
  foldl f y (Right x) = f y x

instance Traversable (Either e) where
  traverse _ (Left x)  = pure (Left x)
  traverse f (Right x) = Right <$> f x

instance Semigroup (Either a b) where
  Left _ <> b = b
  a      <> _ = a

-- deepseq
instance (NFData a, NFData b) => NFData (Either a b) where
  rnf = rnf . toLazy

#if MIN_VERSION_deepseq(1,4,3)
instance (NFData a) => NFData1 (Either a) where
  liftRnf rnfA = liftRnf rnfA . toLazy

instance NFData2 Either where
  liftRnf2 rnfA rnfB = liftRnf2 rnfA rnfB . toLazy
#endif

-- binary
instance (Binary a, Binary b) => Binary (Either a b) where
  put = put . toLazy
  get = toStrict <$> get

-- bifunctors
instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right a) = Right (g a)
  first f = either (Left . f) Right
  second g = either Left (Right . g)

instance Bifoldable Either where
  bifold (Left a) = a
  bifold (Right b) = b
  bifoldMap = either
  bifoldr f _ c (Left a) = f a c
  bifoldr _ g c (Right b) = g b c
  bifoldl f _ c (Left a) = f c a
  bifoldl _ g c (Right b) = g c b

instance Bitraversable Either where
  bitraverse f _ (Left a) = fmap Left (f a)
  bitraverse _ g (Right b) = fmap Right (g b)

-- hashable
instance (Hashable a, Hashable b) => Hashable (Either a b) where
  hashWithSalt salt = hashWithSalt salt . toLazy

instance (Hashable a) => Hashable1 (Either a) where
  liftHashWithSalt hashA salt = liftHashWithSalt hashA salt . toLazy

instance Hashable2 Either where
  liftHashWithSalt2 hashA hashB salt = liftHashWithSalt2 hashA hashB salt . toLazy

-- assoc
#ifdef MIN_VERSION_assoc
instance Assoc Either where
    assoc (Left (Left a))  = Left a
    assoc (Left (Right b)) = Right (Left b)
    assoc (Right c)        = Right (Right c)

    unassoc (Left a)          = Left (Left a)
    unassoc (Right (Left b))  = Left (Right b)
    unassoc (Right (Right c)) = Right c

instance Swap Either where
    swap (Left x) = Right x
    swap (Right x) = Left x
#endif

-- Data.Functor.Classes
#ifdef LIFTED_FUNCTOR_CLASSES
instance Eq2 Either where
  liftEq2 f _ (Left a)  (Left a')  = f a a'
  liftEq2 _ g (Right b) (Right b') = g b b'
  liftEq2 _ _ _         _          = False

instance Eq a => Eq1 (Either a) where
  liftEq = liftEq2 (==)

instance Ord2 Either where
  liftCompare2 f _ (Left a)    (Left a')     = f a a'
  liftCompare2 _ _ (Left _)    _             = LT
  liftCompare2 _ _ _           (Left _)      = GT
  liftCompare2 _ g (Right b)    (Right b')     = g b b'

instance Ord a => Ord1 (Either a) where
  liftCompare = liftCompare2 compare

instance Show a => Show1 (Either a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 Either where
  liftShowsPrec2 sa _ _sb _ d (Left a) = showParen (d > 10)
    $ showString "Left "
    . sa 11 a
  liftShowsPrec2 _sa _ sb _ d (Right b) = showParen (d > 10)
    $ showString "Right "
    . sb 11 b

instance Read2 Either where
  liftReadsPrec2 ra _ rb _ d = readParen (d > 10) $ \s -> cons s
    where
      cons s0 = do
        (ident, s1) <- lex s0
        case ident of
            "Left" ->  do
                (a, s2) <- ra 11 s1
                return (Left a, s2)
            "Right" ->  do
                (b, s2) <- rb 11 s1
                return (Right b, s2)
            _ -> []

instance Read a => Read1 (Either a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList
#else
instance Eq a   => Eq1   (Either a) where eq1        = (==)
instance Ord a  => Ord1  (Either a) where compare1   = compare
instance Show a => Show1 (Either a) where showsPrec1 = showsPrec
instance Read a => Read1 (Either a) where readsPrec1 = readsPrec
#endif
