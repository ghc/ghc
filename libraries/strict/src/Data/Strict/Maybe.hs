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
-- The strict variant of the standard Haskell 'L.Maybe' type and the
-- corresponding variants of the functions from "Data.Maybe".
--
-- Note that in contrast to the standard lazy 'L.Maybe' type, the strict
-- 'Maybe' type is not an applicative functor, and therefore also not a monad.
-- The problem is the /homomorphism/ law, which states that
--
--      @'pure' f '<*>' 'pure' x = 'pure' (f x)  -- must hold for all f@
--
-- This law does not hold for the expected applicative functor instance of
-- 'Maybe', as this instance does not satisfy @pure f \<*\> pure _|_ = pure (f
-- _|_)@ for @f = const@.
--
-----------------------------------------------------------------------------

module Data.Strict.Maybe (
    Maybe(..)
  , isJust
  , isNothing
  , fromJust
  , fromMaybe
  , maybe
  , listToMaybe
  , maybeToList
  , catMaybes
  , mapMaybe
) where

-- import parts explicitly, helps with compatibility
import           Prelude (Functor (..), Eq (..), Ord (..), Show (..), Read (..), Bool (..), (.)
                         ,error, Ordering (..), ($), showString, showParen, return, lex, readParen)
import           Control.Applicative (pure, (<$>))
import           Data.Monoid (Monoid (..))
import           Data.Semigroup (Semigroup (..))
import           Data.Foldable (Foldable (..))
import           Data.Traversable (Traversable (..))

-- Lazy variants
import qualified Prelude             as L

import           Control.DeepSeq     (NFData (..))
import           Data.Binary         (Binary (..))
import           Data.Hashable       (Hashable(..))
import           Data.Hashable.Lifted (Hashable1 (..))
import           GHC.Generics        (Generic)
import           Data.Data           (Data (..), Typeable)


#if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics        (Generic1)
#endif

#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData1 (..))
#endif

#ifdef LIFTED_FUNCTOR_CLASSES
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Read1 (..), Show1 (..))
#else
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Read1 (..), Show1 (..))
#endif

-- | The type of strict optional values.
data Maybe a = Nothing | Just !a
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic
#if __GLASGOW_HASKELL__ >= 706
    , Generic1
#endif
    )

toStrict :: L.Maybe a -> Maybe a
toStrict L.Nothing  = Nothing
toStrict (L.Just x) = Just x

toLazy :: Maybe a -> L.Maybe a
toLazy Nothing  = L.Nothing
toLazy (Just x) = L.Just x

-- | Yields 'True' iff the argument is of the form @Just _@.
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

-- | Yields 'True' iff the argument is 'Nothing'.
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- | Extracts the element out of a 'Just' and throws an error if the argument
-- is 'Nothing'.
fromJust :: Maybe a -> a
fromJust Nothing  = error "Data.Strict.Maybe.fromJust: Nothing"
fromJust (Just x) = x

-- | Given a default value and a 'Maybe', yield the default value if the
-- 'Maybe' argument is 'Nothing' and extract the value out of the 'Just'
-- otherwise.
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just y) = y

-- | Given a default value, a function and a 'Maybe' value, yields the default
-- value if the 'Maybe' value is 'Nothing' and applies the function to the
-- value stored in the 'Just' otherwise.
maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing  = x
maybe _ f (Just y) = f y

-- | Analogous to 'L.listToMaybe' in "Data.Maybe".
listToMaybe :: [a] -> Maybe a
listToMaybe []        =  Nothing
listToMaybe (a:_)     =  Just a

-- | Analogous to 'L.maybeToList' in "Data.Maybe".
maybeToList :: Maybe a -> [a]
maybeToList  Nothing   = []
maybeToList  (Just x)  = [x]

-- | Analogous to 'L.catMaybes' in "Data.Maybe".
catMaybes :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

-- | Analogous to 'L.mapMaybe' in "Data.Maybe".
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) = case f x of
    Nothing -> rs
    Just r  -> r:rs
  where
    rs = mapMaybe f xs

-- Instances
------------

instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> m       = m
  m       <> Nothing = m
  Just x1 <> Just x2 = Just (x1 <> x2)

#if MIN_VERSION_base(4,11,0)
instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing
#else
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing

  Nothing `mappend` m       = m
  m       `mappend` Nothing = m
  Just x1 `mappend` Just x2 = Just (x1 `mappend` x2)
#endif

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Foldable Maybe where
    foldMap _ Nothing  = mempty
    foldMap f (Just x) = f x

instance Traversable Maybe where
    traverse _ Nothing  = pure Nothing
    traverse f (Just x) = Just <$> f x

-- deepseq
instance NFData a => NFData (Maybe a) where
  rnf = rnf . toLazy

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 Maybe where
  liftRnf rnfA = liftRnf rnfA . toLazy
#endif

-- binary
instance Binary a => Binary (Maybe a) where
  put = put . toLazy
  get = toStrict <$> get

-- hashable
instance Hashable a => Hashable (Maybe a) where
  hashWithSalt salt = hashWithSalt salt . toLazy

instance Hashable1 Maybe where
  liftHashWithSalt hashA salt = liftHashWithSalt hashA salt . toLazy

-- Data.Functor.Classes
#ifdef LIFTED_FUNCTOR_CLASSES

instance Eq1 Maybe where
  liftEq f (Just a) (Just a') = f a a'
  liftEq _ Nothing  Nothing   = True
  liftEq _ _        _         = False

instance Ord1 Maybe where
  liftCompare _ Nothing  Nothing   = EQ
  liftCompare _ Nothing  (Just _)  = LT
  liftCompare _ (Just _) Nothing   = GT
  liftCompare f (Just a) (Just a') = f a a'

instance Show1 Maybe where
  liftShowsPrec _  _ _ Nothing = showString "Nothing"
  liftShowsPrec sa _ d (Just a) = showParen (d > 10)
    $ showString "Just "
    . sa 11 a

instance Read1 Maybe where
  liftReadsPrec ra _ d = readParen (d > 10) cons where
    cons s0 = do
      (ident, s1) <- lex s0
      case ident of
        "Nothing" -> return (Nothing, s1)
        "Just"    -> do
          (a, s2) <- ra 11 s1
          return (Just a, s2)
        _         -> []

#else
instance Eq1   Maybe where eq1        = (==)
instance Ord1  Maybe where compare1   = compare
instance Show1 Maybe where showsPrec1 = showsPrec
instance Read1 Maybe where readsPrec1 = readsPrec
#endif
