{- ORMOLU_DISABLE -}
-- Options passed to GHC
{-# OPTIONS_GHC -O2 #-}
-- Options passed to Haddock
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
{-

This module imports the unsafe module 'GHC.Exts' for 'IsList' but does not use
any unsafe features. Therefore, we mark the module as trustworthy.

-}
{-# LANGUAGE Trustworthy #-}
#endif

-- For the IsList and IsString instances
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------

{-|

Module: Data.DList.DNonEmpty.Internal
Copyright: © 2017-2020 Oleg Grenrus, 2020 Sean Leather
License: BSD-3-Clause

Maintainer: sean.leather@gmail.com
Stability: stable

This module includes everything related to 'DNonEmpty' and is not exposed to
users of the @dlist@ package.

-}
{- ORMOLU_ENABLE -}

module Data.DList.DNonEmpty.Internal where

-----------------------------------------------------------------------------

import qualified Control.Applicative as Applicative
import Control.DeepSeq (NFData (..))
import qualified Control.Monad as Monad
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Foldable as Foldable
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Semigroup as Semigroup
import Data.String (IsString (..))
import qualified GHC.Exts as Exts
import qualified Text.Read as Read
import Prelude hiding (head, map, tail)

-----------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
{-|

A non-empty difference list is a pair of a 'head' element and a (possibly empty)
difference list.

Just as 'DList' is a representation of a list, so is @DNonEmpty@ a
representation of a 'NonEmpty'. @DNonEmpty@ supports&#x00A0;\(\mathcal{O}\)(@1@)
'append' and 'snoc' operations, making it useful for replacing frequent
applications of 'Semigroup.<>' on 'NonEmpty' (which is implemented with '++'),
especially if those uses are left-nested (e.g.&#x00A0;@(a __'Semigroup.<>'__ b)
'Semigroup.<>' c@ ).

Unlike 'DList', @DNonEmpty@ is not an abstract type: its constructor is
exported. An alternative definition of @DNonEmpty@ is:

@
newtype DNonEmpty a = DNonEmpty ([a] -> 'NonEmpty' a)
@

This type would need to be abstract to avoid producing @DNonEmpty@ values that
are not isomorphic to 'NonEmpty' values. However, this type would also require
some functions (such as 'map') to be implemented with 'fromNonEmpty' (and thus
'++'), which could introduce efficiencies.

-}
{- ORMOLU_ENABLE -}

infixr 5 :|

data DNonEmpty a = a :| DList a

{- ORMOLU_DISABLE -}
{-|

__@fromNonEmpty xs@__ is a 'DNonEmpty' representing the 'NonEmpty' __@xs@__.

@fromNonEmpty@ obeys the laws:

@
'toNonEmpty' . __fromNonEmpty__ = 'id'
__fromNonEmpty__ . 'toNonEmpty' = 'id'
@

As with 'DList.fromList', this function is implemented with '++'. Repeated uses
of @fromNonEmpty@ are just as inefficient as repeated uses of '++'. If you find
yourself doing some form of the following (possibly indirectly), you may not be
taking advantage of the 'DNonEmpty' representation and library:

@
__fromNonEmpty__ . f . 'toNonEmpty'
@

More likely, you will convert from a 'NonEmpty', perform some operation on the
'DNonEmpty', and convert back to a 'NonEmpty':

@
'toNonEmpty' . g . __fromNonEmpty__
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE fromNonEmpty #-}
fromNonEmpty :: NonEmpty a -> DNonEmpty a
fromNonEmpty ~(x NonEmpty.:| xs) = x :| DList.fromList xs

{- ORMOLU_DISABLE -}
{-|

__@toNonEmpty xs@__ is the 'NonEmpty' represented by __@xs@__.

@toNonEmpty@ obeys the laws:

@
__toNonEmpty__ . 'fromNonEmpty' = 'id'
'fromNonEmpty' . __toNonEmpty__ = 'id'
@

As with 'DList.toList', evaluating @toNonEmpty xs@ may “collapse” the chain of
function composition underlying many 'DList' functions ('DList.append' in
particular) used to construct the 'tail' of @xs@. This may affect any efficiency
you achieved due to laziness in the construction.

-}
{- ORMOLU_ENABLE -}

{-# INLINE toNonEmpty #-}
toNonEmpty :: DNonEmpty a -> NonEmpty a
toNonEmpty ~(x :| xs) = x NonEmpty.:| DList.toList xs

{- ORMOLU_DISABLE -}
{-|

__@toDList xs@__ is the non-empty 'DList' represented by __@xs@__.

@toDList@ obeys the law:

@
__toDList__ (x ':|' xs) = 'DList.cons' x xs
@

Note that this function is used only in this module.

-}
{- ORMOLU_ENABLE -}

toDList :: DNonEmpty a -> DList a
toDList ~(x :| xs) = DList.cons x xs

{- ORMOLU_DISABLE -}
{-|

__@toList xs@__ is the non-empty list represented by __@xs@__.

@toList@ obeys the law:

@
__toList__ xs = 'NonEmpty.toList' ('toNonEmpty' xs)
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE toList #-}
toList :: DNonEmpty a -> [a]
toList = DList.toList . toDList

{- ORMOLU_DISABLE -}
{-|

__@fromList xs@__ is a 'DNonEmpty' representing the list __@xs@__. If @xs@ is
empty, an 'error' is raised.

@fromList@ obeys the law:

@
__fromList__ xs = 'fromNonEmpty' ('NonEmpty.fromList' xs)
@

-}
{- ORMOLU_ENABLE -}

fromList :: [a] -> DNonEmpty a
fromList (x : xs) = x :| DList.fromList xs
fromList [] = error "Data.DList.DNonEmpty.fromList: empty list"

{- ORMOLU_DISABLE -}
{-|

__@singleton x@__ is a 'DNonEmpty' with the single element __@x@__.

@singleton@ obeys the law:

@
'toNonEmpty' (__singleton__ x) = x 'NonEmpty.:|' []
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE singleton #-}
singleton :: a -> DNonEmpty a
singleton x = x :| DList.empty

{- ORMOLU_DISABLE -}
{-|

__@cons x xs@__ is a 'DNonEmpty' with the 'head' __@x@__ and the 'tail' __@xs@__.

\(\mathcal{O}\)(@1@).

@cons@ obeys the law:

@
'toNonEmpty' (__cons__ x xs) = 'NonEmpty.cons' x ('toNonEmpty' xs)
@

-}
{- ORMOLU_ENABLE -}

infixr 9 `cons`

{-# INLINE cons #-}
cons :: a -> DNonEmpty a -> DNonEmpty a
cons x ~(y :| ys) = x :| DList.cons y ys

infixl 9 `snoc`

{- ORMOLU_DISABLE -}
{-|

__@snoc xs x@__ is a 'DNonEmpty' with the initial 'DNonEmpty' __@xs@__ and the
last element __@x@__.

\(\mathcal{O}\)(@1@).

@snoc@ obeys the law:

@
'toNonEmpty' (__snoc__ xs x) = 'toNonEmpty' xs 'Semigroup.<>' (x 'NonEmpty.:|' [])
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE snoc #-}
snoc :: DNonEmpty a -> a -> DNonEmpty a
snoc ~(x :| xs) y = x :| DList.snoc xs y

{- ORMOLU_DISABLE -}
{-|

__@append xs ys@__ is a 'DNonEmpty' obtained from the concatenation of the
elements of __@xs@__ and __@ys@__.

\(\mathcal{O}\)(@1@).

@append@ obeys the law:

@
'toNonEmpty' (__append__ xs ys) = 'toNonEmpty' xs 'Semigroup.<>' 'toNonEmpty' ys
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE append #-}
append :: DNonEmpty a -> DNonEmpty a -> DNonEmpty a
append (x :| xs) ~(y :| ys) = x :| DList.append xs (DList.cons y ys)

{- ORMOLU_DISABLE -}
{-|

__@head xs@__ is the first element of __@xs@__.

\(\mathcal{O}\)(@1@).

@head@ obeys the law:

@
__head__ xs = 'NonEmpty.head' ('toNonEmpty' xs)
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE head #-}
head :: DNonEmpty a -> a
head ~(x :| _) = x

{- ORMOLU_DISABLE -}
{-|

__@tail xs@__ is a 'DList' of the elements in __@xs@__ excluding the first
element.

\(\mathcal{O}\)(@1@).

@tail@ obeys the law:

@
'DList.toList' (__tail__ xs) = 'NonEmpty.tail' ('toNonEmpty' xs)
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE tail #-}
tail :: DNonEmpty a -> DList a
tail ~(_ :| xs) = xs

{- ORMOLU_DISABLE -}
{-|

__@unfoldr f z@__ is the 'DNonEmpty' constructed from the recursive application
of __@f@__. The recursion starts with the seed value __@z@__ and ends when, for
some @z' : b@, @f z' == 'Nothing'@.

\(\mathcal{O}\)(@'NonEmpty.length' ('NonEmpty.unfoldr' f z)@).

@unfoldr@ obeys the law:

@
'toNonEmpty' (__unfoldr__ f z) = 'NonEmpty.unfoldr' f z
@

-}
{- ORMOLU_ENABLE -}

unfoldr :: (b -> (a, Maybe b)) -> b -> DNonEmpty a
unfoldr f z =
  case f z of
    (x, Nothing) -> singleton x
    (x, Just z') -> cons x $ unfoldr f z'

{- ORMOLU_DISABLE -}
{-|

__@map f xs@__ is the 'DNonEmpty' obtained by applying __@f@__ to each element
of __@xs@__.

\(\mathcal{O}\)(@'NonEmpty.length' ('toNonEmpty' xs)@).

@map@ obeys the law:

@
'toNonEmpty' (__map__ f xs) = 'NonEmpty.map' f ('toNonEmpty' xs)
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE map #-}
map :: (a -> b) -> DNonEmpty a -> DNonEmpty b
map f ~(x :| xs) = f x :| DList.map f xs

instance Eq a => Eq (DNonEmpty a) where
  (==) = (==) `on` toNonEmpty

instance Ord a => Ord (DNonEmpty a) where
  compare = compare `on` toNonEmpty

instance Read a => Read (DNonEmpty a) where
  readPrec = Read.parens $
    Read.prec 10 $ do
      Read.Ident "fromNonEmpty" <- Read.lexP
      dl <- Read.readPrec
      return $ fromNonEmpty dl
  readListPrec = Read.readListPrecDefault

instance Show a => Show (DNonEmpty a) where
  showsPrec p dl =
    showParen (p > 10) $
      showString "fromNonEmpty " . shows (toNonEmpty dl)

instance Functor DNonEmpty where
  {-# INLINE fmap #-}
  fmap = map

instance Applicative.Applicative DNonEmpty where
  {-# INLINE pure #-}
  pure = singleton

  {-# INLINE (<*>) #-}
  (<*>) = Monad.ap

instance Monad DNonEmpty where
  ~(x :| xs) >>= k = y :| DList.append ys (xs >>= toDList . k)
    where
      y :| ys = k x

  {-# INLINE return #-}
  return = Applicative.pure

instance Foldable.Foldable DNonEmpty where
  {-# INLINE fold #-}
  fold = Foldable.fold . toNonEmpty

  {-# INLINE foldMap #-}
  foldMap f = Foldable.foldMap f . toNonEmpty

  {-# INLINE foldr #-}
  foldr f x = Foldable.foldr f x . toNonEmpty

  {-# INLINE foldl #-}
  foldl f x = Foldable.foldl f x . toNonEmpty

  {-# INLINE foldr1 #-}
  foldr1 f = Foldable.foldr1 f . toNonEmpty

  {-# INLINE foldl1 #-}
  foldl1 f = Foldable.foldl1 f . toNonEmpty

  {-# INLINE foldl' #-}
  foldl' f x = Foldable.foldl' f x . toNonEmpty

  {-# INLINE foldr' #-}
  foldr' f x = Foldable.foldr' f x . toNonEmpty

  {-# INLINE toList #-}
  toList = toList

instance NFData a => NFData (DNonEmpty a) where
  {-# INLINE rnf #-}
  rnf = rnf . toNonEmpty

{-

The 'IsString' instance is _not_ a flexible instance to allow certain uses of
overloaded strings. See tests/OverloadedStrings.hs for an example and
https://gitlab.haskell.org/ghc/ghc/-/commit/b225b234a6b11e42fef433dcd5d2a38bb4b466bf
for the same change made to the IsString instance for lists.

-}

instance a ~ Char => IsString (DNonEmpty a) where
  {-# INLINE fromString #-}
  fromString = fromList

instance Exts.IsList (DNonEmpty a) where
  type Item (DNonEmpty a) = a

  {-# INLINE fromList #-}
  fromList = fromList

  {-# INLINE toList #-}
  toList = toList

instance Semigroup.Semigroup (DNonEmpty a) where
  {-# INLINE (<>) #-}
  (<>) = append
