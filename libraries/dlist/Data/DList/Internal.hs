{- ORMOLU_DISABLE -}
-- Options passed to GHC
{-# OPTIONS_GHC -O2 #-}
-- Options passed to Haddock
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

{-

We use __GLASGOW_HASKELL__ everywhere, so, rather than check if it's defined in
multiple places, we assert an error here if it is not. Since the rest of the
package depends on this module ('Data.DList.Internal'), we don't perform the
same check everywhere else.

-}
#if !defined(__GLASGOW_HASKELL__)
#error "Your compiler is not GHC. Let us know if dlist can be made to work on it."
#endif

-- For the IsList and IsString instances
{-# LANGUAGE TypeFamilies #-}

-- CPP: GHC >= 7.8 for pattern synonyms, Safe Haskell, view patterns
#if __GLASGOW_HASKELL__ >= 708
{- ORMOLU_ENABLE -}
{-# LANGUAGE PatternSynonyms #-}
{-

The 'Data.DList.Internal' module exports 'UnsafeDList' and 'unsafeApplyDList',
which allow breaking the invariant of the 'DList' newtype. Therefore, we
explicitly mark 'Data.DList.Internal' as unsafe.

-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ViewPatterns #-}
{- ORMOLU_DISABLE -}
#endif

-----------------------------------------------------------------------------

{-|

Module: Data.DList.Internal
Copyright: © 2006-2009 Don Stewart, 2013-2020 Sean Leather
License: BSD-3-Clause

Maintainer: sean.leather@gmail.com
Stability: stable

This module includes everything related to 'DList' and is not exposed to users
of the @dlist@ package.

-}
{- ORMOLU_ENABLE -}

module Data.DList.Internal where

-----------------------------------------------------------------------------

import qualified Control.Applicative as Applicative
import Control.DeepSeq (NFData (..))
import qualified Control.Monad as Monad
-- CPP: base >= 4.9 for MonadFail
-- CPP: base >= 4.13 for MonadFail exported from Control.Monad
#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,13,0)
import qualified Control.Monad.Fail as Monad
#endif
import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Monoid as Monoid
-- CPP: base >= 4.9 for Semigroup
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semigroup
#endif
import Data.String (IsString (..))
import qualified Data.Traversable as Traversable
-- CPP: GHC >= 7.8 for IsList
#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts as Exts
#endif
import qualified Text.Read as Read
import Prelude hiding (concat, foldr, head, map, replicate, tail)

-----------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
{-|

A difference list is an abstraction representing a list that
supports&#x00A0;\(\mathcal{O}\)(@1@) 'append' and 'snoc' operations, making it
useful for replacing frequent applications of '++' such as logging and pretty
printing (esp. if those uses of '++' are left-nested).

-}
{- ORMOLU_ENABLE -}

newtype DList a = UnsafeDList {unsafeApplyDList :: [a] -> [a]}

{- ORMOLU_DISABLE -}
{-|

__@fromList xs@__ is a 'DList' representing the list __@xs@__.

@fromList@ obeys the laws:

@
'toList' . __fromList__ = 'id'
__fromList__ . 'toList' = 'id'
@

This function is implemented with '++'. Repeated uses of @fromList@ are just as
inefficient as repeated uses of '++'. If you find yourself doing some form of
the following (possibly indirectly), you may not be taking advantage of the
'DList' representation and library:

@
__fromList__ . f . 'toList'
@

More likely, you will convert from a list, perform some operation on the
'DList', and convert back to a list:

@
'toList' . g . __fromList__
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE fromList #-}
fromList :: [a] -> DList a
fromList = UnsafeDList . (++)

{- ORMOLU_DISABLE -}
{-|

__@toList xs@__ is the list represented by __@xs@__.

@toList@ obeys the laws:

@
__toList__ . 'fromList' = 'id'
'fromList' . __toList__ = 'id'
@

Evaluating @toList xs@ may “collapse” the chain of function composition
underlying many 'DList' functions ('append' in particular) used to construct
@xs@. This may affect any efficiency you achieved due to laziness in the
construction.

-}
{- ORMOLU_ENABLE -}

{-# INLINE toList #-}
toList :: DList a -> [a]
toList = ($ []) . unsafeApplyDList

-- CPP: GHC >= 7.8 for pattern synonyms
#if __GLASGOW_HASKELL__ >= 708

-- CPP: GHC >= 7.10 for pattern synonym signatures

{- ORMOLU_DISABLE -}
{-|

A unidirectional pattern synonym for 'empty'. This is implemented with 'toList'.

-}
{- ORMOLU_ENABLE -}

#if __GLASGOW_HASKELL__ >= 710
pattern Nil :: DList a
#endif
pattern Nil <- (toList -> [])

{- ORMOLU_DISABLE -}
{-|

A unidirectional pattern synonym for 'cons'. This is implemented with 'toList'.

-}
{- ORMOLU_ENABLE -}

#if __GLASGOW_HASKELL__ >= 710
pattern Cons :: a -> [a] -> DList a
#endif
pattern Cons x xs <- (toList -> x : xs)

#endif

{- ORMOLU_DISABLE -}
{-|

__@apply xs ys@__ is the list represented by the __@xs@__ after appending
__@ys@__ to it.

\(\mathcal{O}\)(@1@).

@apply@ obeys the law:

@
__apply__ xs ys = 'toList' xs '++' ys
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE apply #-}
apply :: DList a -> [a] -> [a]
apply = unsafeApplyDList

{- ORMOLU_DISABLE -}
{-|

__@empty@__ is a 'DList' with no elements.

@empty@ obeys the law:

@
'toList' __empty__ = []
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE empty #-}
empty :: DList a
empty = UnsafeDList id

{- ORMOLU_DISABLE -}
{-|

__@singleton x@__ is a 'DList' with the single element __@x@__.

@singleton@ obeys the law:

@
'toList' (__singleton__ x) = [x]
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE singleton #-}
singleton :: a -> DList a
singleton = UnsafeDList . (:)

{- ORMOLU_DISABLE -}
{-|

__@cons x xs@__ is a 'DList' with the 'head' __@x@__ and the 'tail' __@xs@__.

\(\mathcal{O}\)(@1@).

@cons@ obeys the law:

@
'toList' (__cons__ x xs) = x : 'toList' xs
@

-}
{- ORMOLU_ENABLE -}

infixr 9 `cons`

{-# INLINE cons #-}
cons :: a -> DList a -> DList a
cons x xs = UnsafeDList $ (x :) . unsafeApplyDList xs

infixl 9 `snoc`

{- ORMOLU_DISABLE -}
{-|

__@snoc xs x@__ is a 'DList' with the initial 'DList' __@xs@__ and the last
element __@x@__.

\(\mathcal{O}\)(@1@).

@snoc@ obeys the law:

@
'toList' (__snoc__ xs x) = 'toList' xs '++' [x]
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE snoc #-}
snoc :: DList a -> a -> DList a
snoc xs x = UnsafeDList $ unsafeApplyDList xs . (x :)

{- ORMOLU_DISABLE -}
{-|

__@append xs ys@__ is a 'DList' obtained from the concatenation of the elements
of __@xs@__ and __@ys@__.

\(\mathcal{O}\)(@1@).

@append@ obeys the law:

@
'toList' (__append__ xs ys) = 'toList' xs '++' 'toList' ys
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE append #-}
append :: DList a -> DList a -> DList a
append xs ys = UnsafeDList $ unsafeApplyDList xs . unsafeApplyDList ys

{- ORMOLU_DISABLE -}
{-|

__@concat xss@__ is a 'DList' representing the concatenation of all 'DList's in
the list __@xss@__.

\(\mathcal{O}\)(@'length' xss@).

@concat@ obeys the law:

@
'toList' (__concat__ xss) = 'List.concat' ('List.map' 'toList' xss)
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE concat #-}
concat :: [DList a] -> DList a
concat = List.foldr append empty

{- ORMOLU_DISABLE -}
{-|

__@replicate n x@__ is a 'DList' of length __@n@__ with __@x@__ as the value of
every element.

\(\mathcal{O}\)(@n@).

@replicate@ obeys the law:

@
'toList' (__replicate__ n x) = 'List.replicate' n x
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE replicate #-}
replicate :: Int -> a -> DList a
replicate n x = UnsafeDList $ \xs ->
  let go m
        | m <= 0 = xs
        | otherwise = x : go (m -1)
   in go n

{- ORMOLU_DISABLE -}
{-|

__@head xs@__ is the first element of __@xs@__. If @xs@ is empty, an 'error' is
raised.

\(\mathcal{O}\)(@1@).

@head@ obeys the law:

@
__head__ xs = 'List.head' ('toList' xs)
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE head #-}
head :: DList a -> a
head xs = case toList xs of
  x : _ -> x
  [] -> error "Data.DList.head: empty DList"

{- ORMOLU_DISABLE -}
{-|

__@tail xs@__ is a list of the elements in __@xs@__ excluding the first element.
If @xs@ is empty, an 'error' is raised.

\(\mathcal{O}\)(@'length' ('toList' xs)@).

@tail@ obeys the law:

@
__tail__ xs = 'List.tail' ('toList' xs)
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE tail #-}
tail :: DList a -> [a]
tail xs = case toList xs of
  _ : ys -> ys
  [] -> error "Data.DList.tail: empty DList"

{- ORMOLU_DISABLE -}
{-|

__@unfoldr f z@__ is the 'DList' constructed from the recursive application of
__@f@__. The recursion starts with the seed value __@z@__ and ends when, for
some @z' : b@, @f z' == 'Nothing'@.

\(\mathcal{O}\)(@'length' ('List.unfoldr' f z)@).

@unfoldr@ obeys the law:

@
'toList' (__unfoldr__ f z) = 'List.unfoldr' f z
@

-}
{- ORMOLU_ENABLE -}

unfoldr :: (b -> Maybe (a, b)) -> b -> DList a
unfoldr f z =
  case f z of
    Nothing -> empty
    Just (x, z') -> cons x $ unfoldr f z'

{- ORMOLU_DISABLE -}
{-|

__@foldr f z xs@__ is the right-fold of __@f@__ over __@xs@__.

\(\mathcal{O}\)(@'length' ('toList' xs)@).

@foldr@ obeys the law:

@
__foldr__ f z xs = 'List.foldr' f z ('toList' xs)
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> DList a -> b
foldr f z = List.foldr f z . toList

{- ORMOLU_DISABLE -}
{-|

__@map f xs@__ is the 'DList' obtained by applying __@f@__ to each element of
__@xs@__.

\(\mathcal{O}\)(@'length' ('toList' xs)@).

@map@ obeys the law:

@
'toList' (__map__ f xs) = 'List.map' f ('toList' xs)
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE map #-}
map :: (a -> b) -> DList a -> DList b
map f = foldr (cons . f) empty

{- ORMOLU_DISABLE -}
{-|

__@intercalate xs xss@__ is the concatenation of __@xss@__ after the insertion
of __@xs@__ between every pair of
elements.

\(\mathcal{O}\)(@'length' xss@).

@intercalate@ obeys the law:

@
'toList' (__intercalate__ xs xss) = 'List.intercalate' ('toList' xs) ('map' 'toList' xss)
@

-}
{- ORMOLU_ENABLE -}

{-# INLINE intercalate #-}
intercalate :: DList a -> [DList a] -> DList a
intercalate sep = concat . List.intersperse sep

instance Eq a => Eq (DList a) where
  (==) = (==) `on` toList

instance Ord a => Ord (DList a) where
  compare = compare `on` toList

-- The 'Read' and 'Show' instances were adapted from 'Data.Sequence'.

instance Read a => Read (DList a) where
  readPrec = Read.parens $
    Read.prec 10 $ do
      Read.Ident "fromList" <- Read.lexP
      dl <- Read.readPrec
      return (fromList dl)
  readListPrec = Read.readListPrecDefault

instance Show a => Show (DList a) where
  showsPrec p dl =
    showParen (p > 10) $
      showString "fromList " . shows (toList dl)

instance Monoid.Monoid (DList a) where
  {-# INLINE mempty #-}
  mempty = empty

-- CPP: base >= 4.11 for Semigroup as a superclass of Monoid
#if MIN_VERSION_base(4,11,0)

#else

  {-# INLINE mappend #-}
-- CPP: base >= 4.9 for Semigroup in base
#if MIN_VERSION_base(4,9,0)
  -- Canonical definition
  mappend = (Semigroup.<>)
#else
  mappend = append
#endif

#endif

instance Functor DList where
  {-# INLINE fmap #-}
  fmap = map

instance Applicative.Applicative DList where
  {-# INLINE pure #-}
  pure = singleton

  {-# INLINE (<*>) #-}
  (<*>) = Monad.ap

instance Applicative.Alternative DList where
  {-# INLINE empty #-}
  empty = empty

  {-# INLINE (<|>) #-}
  (<|>) = append

instance Monad DList where
  {-# INLINE (>>=) #-}
  m >>= k =
    -- = concat (toList (fmap k m))
    -- = (concat . toList . fromList . List.map k . toList) m
    -- = concat . List.map k . toList $ m
    -- = List.foldr append empty . List.map k . toList $ m
    -- = List.foldr (append . k) empty . toList $ m
    foldr (append . k) empty m

  {-# INLINE return #-}
  return = Applicative.pure

-- CPP: base < 4.13 for fail in Monad
#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail _ = empty
#endif

-- CPP: base >= 4.9 for MonadFail
#if MIN_VERSION_base(4,9,0)
instance Monad.MonadFail DList where
  {-# INLINE fail #-}
  fail _ = empty
#endif

instance Monad.MonadPlus DList where
  {-# INLINE mzero #-}
  mzero = empty

  {-# INLINE mplus #-}
  mplus = append

instance Foldable.Foldable DList where
  {-# INLINE fold #-}
  fold = Monoid.mconcat . toList

  {-# INLINE foldMap #-}
  foldMap f = Foldable.foldMap f . toList

  {-# INLINE foldr #-}
  foldr f x = List.foldr f x . toList

  {-# INLINE foldl #-}
  foldl f x = List.foldl f x . toList

  {-# INLINE foldr1 #-}
  foldr1 f = List.foldr1 f . toList

  {-# INLINE foldl1 #-}
  foldl1 f = List.foldl1 f . toList

-- CPP: GHC >= 7.6 for foldl', foldr' in Foldable
#if __GLASGOW_HASKELL__ >= 706
  {-# INLINE foldl' #-}
  foldl' f x = List.foldl' f x . toList

  {-# INLINE foldr' #-}
  foldr' f x = Foldable.foldr' f x . toList
#endif

-- CPP: base >= 4.8 for toList in Foldable
#if MIN_VERSION_base(4,8,0)
  {-# INLINE toList #-}
  toList = Data.DList.Internal.toList
#endif

instance Traversable.Traversable DList where
  {-# INLINE traverse #-}
  traverse f = foldr cons_f (Applicative.pure empty)
    where
      cons_f x = Applicative.liftA2 cons (f x)

instance NFData a => NFData (DList a) where
  {-# INLINE rnf #-}
  rnf = rnf . toList

{-

The 'IsString' instance is _not_ a flexible instance to allow certain uses of
overloaded strings. See tests/OverloadedStrings.hs for an example and
https://gitlab.haskell.org/ghc/ghc/-/commit/b225b234a6b11e42fef433dcd5d2a38bb4b466bf
for the same change made to the IsString instance for lists.

-}

instance a ~ Char => IsString (DList a) where
  {-# INLINE fromString #-}
  fromString = fromList

-- CPP: GHC >= 7.8 for IsList
#if __GLASGOW_HASKELL__ >= 708
instance Exts.IsList (DList a) where
  type Item (DList a) = a

  {-# INLINE fromList #-}
  fromList = fromList

  {-# INLINE toList #-}
  toList = toList
#endif

{-

We use 'compare n 0' in the definition of 'Semigroup.stimes' since the same
expression is used in 'Semigroup.stimesMonoid' and we should get a lazy
advantage. However, we prefer the error to be sourced here instead of
'Semigroup.stimesMonoid'.

-}

-- CPP: base >= 4.9 for Semigroup
#if MIN_VERSION_base(4,9,0)
instance Semigroup.Semigroup (DList a) where
  {-# INLINE (<>) #-}
  (<>) = append

  stimes n = case compare n 0 of
    LT -> error "Data.DList.stimes: negative multiplier"
    _ -> Semigroup.stimesMonoid n
#endif
