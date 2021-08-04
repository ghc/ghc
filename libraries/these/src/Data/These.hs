{-# LANGUAGE CPP                #-}
-- | The 'These' type and associated operations. Now enhanced with "Control.Lens" magic!
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Safe               #-}

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

module Data.These (
      These(..)

    -- * Functions to get rid of 'These'
    , these
    , fromThese
    , mergeThese
    , mergeTheseWith

    -- * Partition
    , partitionThese
    , partitionHereThere
    , partitionEithersNE

    -- * Distributivity
    --
    -- | This distributivity combinators aren't isomorphisms!
    , distrThesePair
    , undistrThesePair
    , distrPairThese
    , undistrPairThese
    ) where

import Control.Applicative  (Applicative (..), (<$>))
import Control.DeepSeq      (NFData (..))
import Data.Bifoldable      (Bifoldable (..))
import Data.Bifunctor       (Bifunctor (..))
import Data.Binary          (Binary (..))
import Data.Bitraversable   (Bitraversable (..))
import Data.Data            (Data, Typeable)
import Data.Either          (partitionEithers)
import Data.Foldable        (Foldable (..))
import Data.Hashable        (Hashable (..))
import Data.Hashable.Lifted (Hashable1 (..), Hashable2 (..))
import Data.List.NonEmpty   (NonEmpty (..))
import Data.Monoid          (Monoid (..))
import Data.Semigroup       (Semigroup (..))
import Data.Traversable     (Traversable (..))
import GHC.Generics         (Generic)
import Prelude
       (Bool (..), Either (..), Eq (..), Functor (..), Int, Monad (..),
       Ord (..), Ordering (..), Read (..), Show (..), fail, id, lex, readParen,
       seq, showParen, showString, ($), (&&), (.))

#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData1 (..), NFData2 (..))
#endif

#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif

#ifdef MIN_VERSION_assoc
import Data.Bifunctor.Assoc (Assoc (..))
import Data.Bifunctor.Swap  (Swap (..))
#endif

#ifdef LIFTED_FUNCTOR_CLASSES
import Data.Functor.Classes
       (Eq1 (..), Eq2 (..), Ord1 (..), Ord2 (..), Read1 (..), Read2 (..),
       Show1 (..), Show2 (..))
#else
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Read1 (..), Show1 (..))
#endif

-- $setup
-- >>> import Control.Lens

-- --------------------------------------------------------------------------
-- | The 'These' type represents values with two non-exclusive possibilities.
--
--   This can be useful to represent combinations of two values, where the
--   combination is defined if either input is. Algebraically, the type
--   @'These' A B@ represents @(A + B + AB)@, which doesn't factor easily into
--   sums and products--a type like @'Either' A (B, 'Maybe' A)@ is unclear and
--   awkward to use.
--
--   'These' has straightforward instances of 'Functor', 'Monad', &c., and
--   behaves like a hybrid error/writer monad, as would be expected.
--
--   For zipping and unzipping of structures with 'These' values, see
--   "Data.Align".
data These a b = This a | That b | These a b
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic
#if __GLASGOW_HASKELL__ >= 706
    , Generic1
#endif
    )

-------------------------------------------------------------------------------
-- Eliminators
-------------------------------------------------------------------------------

-- | Case analysis for the 'These' type.
these :: (a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c
these l _ _ (This a) = l a
these _ r _ (That x) = r x
these _ _ lr (These a x) = lr a x

-- | Takes two default values and produces a tuple.
fromThese :: a -> b -> These a b -> (a, b)
fromThese x y = these (`pair` y) (x `pair`) pair where
    pair = (,)

-- | Coalesce with the provided operation.
mergeThese :: (a -> a -> a) -> These a a -> a
mergeThese = these id id

-- | 'bimap' and coalesce results with the provided operation.
mergeTheseWith :: (a -> c) -> (b -> c) -> (c -> c -> c) -> These a b -> c
mergeTheseWith f g op t = mergeThese op $ bimap f g t

-------------------------------------------------------------------------------
-- Partitioning
-------------------------------------------------------------------------------

-- | Select each constructor and partition them into separate lists.
partitionThese :: [These a b] -> ([a], [b], [(a, b)])
partitionThese []     = ([], [], [])
partitionThese (t:ts) = case t of
    This x    -> (x : xs,     ys,         xys)
    That y    -> (    xs, y : ys,         xys)
    These x y -> (    xs,     ys, (x,y) : xys)
  where
    ~(xs,ys,xys) = partitionThese ts

-- | Select 'here' and 'there' elements and partition them into separate lists.
--
-- @since 0.8
partitionHereThere :: [These a b] -> ([a], [b])
partitionHereThere []     = ([], [])
partitionHereThere (t:ts) = case t of
    This x     -> (x : xs,     ys)
    That y     -> (    xs, y : ys)
    These x  y -> (x : xs, y : ys)
  where
    ~(xs,ys) = partitionHereThere ts

-- | Like 'partitionEithers' but for 'NonEmpty' types.
--
-- * either all are 'Left'
-- * either all are 'Right'
-- * or there is both 'Left' and 'Right' stuff
--
-- /Note:/ this is not online algorithm. In the worst case it will traverse
-- the whole list before deciding the result constructor.
--
-- >>> partitionEithersNE $ Left 'x' :| [Right 'y']
-- These ('x' :| "") ('y' :| "")
--
-- >>> partitionEithersNE $ Left 'x' :| map Left "yz"
-- This ('x' :| "yz")
--
-- @since 1.0.1
partitionEithersNE :: NonEmpty (Either a b) -> These (NonEmpty a) (NonEmpty b)
partitionEithersNE (x :| xs) = case (x, ls, rs) of
    (Left y,  ys,   [])   -> This (y :| ys)
    (Left y,  ys,   z:zs) -> These (y :| ys) (z :| zs)
    (Right z, [],   zs)   -> That (z :| zs)
    (Right z, y:ys, zs)   -> These (y :| ys) (z :| zs)
  where
    (ls, rs) = partitionEithers xs


-------------------------------------------------------------------------------
-- Distributivity
-------------------------------------------------------------------------------

distrThesePair :: These (a, b) c -> (These a c, These b c)
distrThesePair (This (a, b))    = (This a, This b)
distrThesePair (That c)         = (That c, That c)
distrThesePair (These (a, b) c) = (These a c, These b c)

undistrThesePair :: (These a c, These b c) -> These (a, b) c
undistrThesePair (This a,    This b)    = This (a, b)
undistrThesePair (That c,    That _)    = That c
undistrThesePair (These a c, These b _) = These (a, b) c
undistrThesePair (This _,    That c)    = That c
undistrThesePair (This a,    These b c) = These (a, b) c
undistrThesePair (That c,    This _)    = That c
undistrThesePair (That c,    These _ _) = That c
undistrThesePair (These a c, This b)    = These (a, b) c
undistrThesePair (These _ c, That _)    = That c


distrPairThese :: (These a b, c) -> These (a, c) (b, c)
distrPairThese (This a,    c) = This (a, c)
distrPairThese (That b,    c) = That (b, c)
distrPairThese (These a b, c) = These (a, c) (b, c)

undistrPairThese :: These (a, c) (b, c) -> (These a b, c)
undistrPairThese (This (a, c))         = (This a, c)
undistrPairThese (That (b, c))         = (That b, c)
undistrPairThese (These (a, c) (b, _)) = (These a b, c)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------



instance (Semigroup a, Semigroup b) => Semigroup (These a b) where
    This  a   <> This  b   = This  (a <> b)
    This  a   <> That    y = These  a             y
    This  a   <> These b y = These (a <> b)       y
    That    x <> This  b   = These       b   x
    That    x <> That    y = That           (x <> y)
    That    x <> These b y = These       b  (x <> y)
    These a x <> This  b   = These (a <> b)  x
    These a x <> That    y = These  a       (x <> y)
    These a x <> These b y = These (a <> b) (x <> y)

instance Functor (These a) where
    fmap _ (This x) = This x
    fmap f (That y) = That (f y)
    fmap f (These x y) = These x (f y)

instance Foldable (These a) where
    foldr _ z (This _) = z
    foldr f z (That x) = f x z
    foldr f z (These _ x) = f x z

instance Traversable (These a) where
    traverse _ (This a) = pure $ This a
    traverse f (That x) = That <$> f x
    traverse f (These a x) = These a <$> f x
    sequenceA (This a) = pure $ This a
    sequenceA (That x) = That <$> x
    sequenceA (These a x) = These a <$> x

instance Bifunctor These where
    bimap f _ (This  a  ) = This (f a)
    bimap _ g (That    x) = That (g x)
    bimap f g (These a x) = These (f a) (g x)

instance Bifoldable These where
    bifold = these id id mappend
    bifoldr f g z = these (`f` z) (`g` z) (\x y -> x `f` (y `g` z))
    bifoldl f g z = these (z `f`) (z `g`) (\x y -> (z `f` x) `g` y)

instance Bitraversable These where
    bitraverse f _ (This x) = This <$> f x
    bitraverse _ g (That x) = That <$> g x
    bitraverse f g (These x y) = These <$> f x <*> g y

instance (Semigroup a) => Applicative (These a) where
    pure = That
    This  a   <*> _         = This a
    That    _ <*> This  b   = This b
    That    f <*> That    x = That (f x)
    That    f <*> These b x = These b (f x)
    These a _ <*> This  b   = This (a <> b)
    These a f <*> That    x = These a (f x)
    These a f <*> These b x = These (a <> b) (f x)


instance (Semigroup a) => Monad (These a) where
    return = pure
    This  a   >>= _ = This a
    That    x >>= k = k x
    These a x >>= k = case k x of
                          This  b   -> This  (a <> b)
                          That    y -> These a y
                          These b y -> These (a <> b) y

-------------------------------------------------------------------------------
-- Data.Functor.Classes
-------------------------------------------------------------------------------

#ifdef LIFTED_FUNCTOR_CLASSES
-- | @since 1.1.1
instance Eq2 These where
  liftEq2 f _ (This a)    (This a')     = f a a'
  liftEq2 _ g (That b)    (That b')     = g b b'
  liftEq2 f g (These a b) (These a' b') = f a a' && g b b'
  liftEq2 _ _ _           _             = False

-- | @since 1.1.1
instance Eq a => Eq1 (These a) where
  liftEq = liftEq2 (==)

-- | @since 1.1.1
instance Ord2 These where
  liftCompare2 f _ (This a)    (This a')     = f a a'
  liftCompare2 _ _ (This _)    _             = LT
  liftCompare2 _ _ _           (This _)      = GT
  liftCompare2 _ g (That b)    (That b')     = g b b'
  liftCompare2 _ _ (That _)    _             = LT
  liftCompare2 _ _ _           (That _)      = GT
  liftCompare2 f g (These a b) (These a' b') = f a a' `mappend` g b b'

-- | @since 1.1.1
instance Ord a => Ord1 (These a) where
  liftCompare = liftCompare2 compare

-- | @since 1.1.1
instance Show a => Show1 (These a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

-- | @since 1.1.1
instance Show2 These where
  liftShowsPrec2 sa _ _sb _ d (This a) = showParen (d > 10)
    $ showString "This "
    . sa 11 a
  liftShowsPrec2 _sa _ sb _ d (That b) = showParen (d > 10)
    $ showString "That "
    . sb 11 b
  liftShowsPrec2 sa _ sb _ d (These a b) = showParen (d > 10)
    $ showString "These "
    . sa 11 a
    . showString " "
    . sb 11 b

-- | @since 1.1.1
instance Read2 These where
  liftReadsPrec2 ra _ rb _ d = readParen (d > 10) $ \s -> cons s
    where
      cons s0 = do
        (ident, s1) <- lex s0
        case ident of
            "This" ->  do
                (a, s2) <- ra 11 s1
                return (This a, s2)
            "That" ->  do
                (b, s2) <- rb 11 s1
                return (That b, s2)
            "These" -> do
                (a, s2) <- ra 11 s1
                (b, s3) <- rb 11 s2
                return (These a b, s3)
            _ -> []

-- | @since 1.1.1
instance Read a => Read1 (These a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList

#else
-- | @since 1.1.1
instance Eq a   => Eq1   (These a) where eq1        = (==)
-- | @since 1.1.1
instance Ord a  => Ord1  (These a) where compare1   = compare
-- | @since 1.1.1
instance Show a => Show1 (These a) where showsPrec1 = showsPrec
-- | @since 1.1.1
instance Read a => Read1 (These a) where readsPrec1 = readsPrec
#endif

-------------------------------------------------------------------------------
-- assoc
-------------------------------------------------------------------------------

#ifdef MIN_VERSION_assoc
-- | @since 0.8
instance Swap These where
    swap (This a)    = That a
    swap (That b)    = This b
    swap (These a b) = These b a

-- | @since 0.8
instance Assoc These where
    assoc (This (This a))       = This a
    assoc (This (That b))       = That (This b)
    assoc (That c)              = That (That c)
    assoc (These (That b) c)    = That (These b c)
    assoc (This (These a b))    = These a (This b)
    assoc (These (This a) c)    = These a (That c)
    assoc (These (These a b) c) = These a (These b c)

    unassoc (This a)              = This (This a)
    unassoc (That (This b))       = This (That b)
    unassoc (That (That c))       = That c
    unassoc (That (These b c))    = These (That b) c
    unassoc (These a (This b))    = This (These a b)
    unassoc (These a (That c))    = These (This a) c
    unassoc (These a (These b c)) = These (These a b) c
#endif

-------------------------------------------------------------------------------
-- deepseq
-------------------------------------------------------------------------------

-- | @since 0.7.1
instance (NFData a, NFData b) => NFData (These a b) where
    rnf (This a)    = rnf a
    rnf (That b)    = rnf b
    rnf (These a b) = rnf a `seq` rnf b

#if MIN_VERSION_deepseq(1,4,3)
-- | @since 1.1.1
instance NFData a => NFData1 (These a) where
    liftRnf _rnfB (This a)    = rnf a
    liftRnf  rnfB (That b)    = rnfB b
    liftRnf  rnfB (These a b) = rnf a `seq` rnfB b

-- | @since 1.1.1
instance NFData2 These where
    liftRnf2  rnfA _rnfB (This a)    = rnfA a
    liftRnf2 _rnfA  rnfB (That b)    = rnfB b
    liftRnf2  rnfA  rnfB (These a b) = rnfA a `seq` rnfB b
#endif

-------------------------------------------------------------------------------
-- binary
-------------------------------------------------------------------------------

-- | @since 0.7.1
instance (Binary a, Binary b) => Binary (These a b) where
    put (This a)    = put (0 :: Int) >> put a
    put (That b)    = put (1 :: Int) >> put b
    put (These a b) = put (2 :: Int) >> put a >> put b

    get = do
        i <- get
        case (i :: Int) of
            0 -> This <$> get
            1 -> That <$> get
            2 -> These <$> get <*> get
            _ -> fail "Invalid These index"

-------------------------------------------------------------------------------
-- hashable
-------------------------------------------------------------------------------

instance (Hashable a, Hashable b) => Hashable (These a b) where
    hashWithSalt salt (This a) =
        salt `hashWithSalt` (0 :: Int) `hashWithSalt` a
    hashWithSalt salt (That b) =
        salt `hashWithSalt` (1 :: Int) `hashWithSalt` b
    hashWithSalt salt (These a b) =
        salt `hashWithSalt` (2 :: Int) `hashWithSalt` a `hashWithSalt` b

-- | @since 1.1.1
instance Hashable a => Hashable1 (These a) where
    liftHashWithSalt _hashB salt (This a) =
        salt `hashWithSalt` (0 :: Int) `hashWithSalt` a
    liftHashWithSalt  hashB salt (That b) =
        (salt `hashWithSalt` (1 :: Int)) `hashB` b
    liftHashWithSalt  hashB salt (These a b) =
        (salt `hashWithSalt` (2 :: Int) `hashWithSalt` a) `hashB` b

-- | @since 1.1.1
instance Hashable2 These where
    liftHashWithSalt2  hashA _hashB salt (This a) =
        (salt `hashWithSalt` (0 :: Int)) `hashA` a
    liftHashWithSalt2 _hashA  hashB salt (That b) =
        (salt `hashWithSalt` (1 :: Int)) `hashB` b
    liftHashWithSalt2  hashA  hashB salt (These a b) =
        (salt `hashWithSalt` (2 :: Int)) `hashA` a `hashB` b
