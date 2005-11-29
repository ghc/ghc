-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- This module describes a structure intermediate between a functor and
-- a monad: it provides pure expressions and sequencing, but no binding.
-- (Technically, a strong lax monoidal functor.)  For more details, see
-- <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>.
--
-- This interface was introduced for parsers by Niklas R&#xF6;jemo, because
-- it admits more sharing than the monadic interface.  The names here are
-- mostly based on recent parsing work by Doaitse Swierstra.
--
-- This class is also useful with instances of the
-- 'Data.Traversable.Traversable' class.

module Control.Applicative (
	-- * Applicative functors
	Applicative(..),
	-- * Instances
	WrappedMonad(..), Const(..), ZipList(..),
	-- * Utility functions
	(<$), (*>), (<*), (<**>),
	liftA, liftA2, liftA3
	) where

#ifdef __HADDOCK__
import Prelude
#endif

import Control.Monad (liftM, ap)
import Data.Monoid (Monoid(..))

infixl 4 <$>, <$
infixl 4 <*>, <*, *>, <**>

-- | A functor with application.
--
-- Instances should satisfy the following laws:
--
-- [/identity/]
--	@'pure' 'id' '<*>' v = v@
--
-- [/composition/]
--	@'pure' (.) '<*>' u '<*>' v '<*>' w = u '<*>' (v '<*>' w)@
--
-- [/homomorphism/]
--	@'pure' f '<*>' 'pure' x = 'pure' (f x)@
--
-- [/interchange/]
--	@u '<*>' 'pure' y = 'pure' ('$' y) '<*>' u@
--
-- [/pure application/]
--	@f '<$>' v = 'pure' f '<*>' v@
--
-- Minimal complete definition: 'pure' and '<*>'.
--
-- If @f@ is also a 'Functor', define @('<$>') = 'fmap'@.
-- If it is also a 'Monad', define @'pure' = 'return'@ and @('<*>') = 'ap'@.

class Applicative f where
	-- | Lift a value.
	pure :: a -> f a

        -- | Sequential application.
	(<*>) :: f (a -> b) -> f a -> f b

	-- | Map a function over an action.
	(<$>) :: (a -> b) -> f a -> f b
	f <$> v = pure f <*> v

-- instances for Prelude types

instance Applicative Maybe where
	pure = return
	(<*>) = ap

instance Applicative [] where
	pure = return
	(<*>) = ap

instance Applicative IO where
	pure = return
	(<*>) = ap

instance Applicative ((->) a) where
	pure = const
	(<*>) f g x = f x (g x)

-- new instances

newtype WrappedMonad m a = WrapMonad { unwrapMonad :: m a }

instance Monad m => Applicative (WrappedMonad m) where
	pure = WrapMonad . return
	WrapMonad f <*> WrapMonad v = WrapMonad (f `ap` v)
	f <$> WrapMonad v = WrapMonad (liftM f v)

newtype Const a b = Const { getConst :: a }

instance Monoid m => Applicative (Const m) where
	pure _ = Const mempty
	Const f <*> Const v = Const (f `mappend` v)
	_ <$> Const v = Const v

-- | Lists, but with an 'Applicative' functor based on zipping, so that
--
-- @f '<$>' 'ZipList' xs1 '<*>' ... '<*>' 'ZipList' xsn = 'ZipList' (zipWithn f xs1 ... xsn)@
--
newtype ZipList a = ZipList { getZipList :: [a] }

instance Applicative ZipList where
	pure x = ZipList (repeat x)
	ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)
	f <$> ZipList xs = ZipList (map f xs)

-- extra functions

-- | Replace the value.
(<$) :: Applicative f => a -> f b -> f a
(<$) = (<$>) . const
 
-- | Sequence actions, discarding the value of the first argument.
(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (const id)
 
-- | Sequence actions, discarding the value of the second argument.
(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const
 
-- | A variant of '<*>' with the arguments reversed.
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))

-- | A synonym for '<$>'.
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = f <$> a

-- | Lift a binary function to actions.
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

-- | Lift a ternary function to actions.
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c
