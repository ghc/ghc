{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Class of data structures that can be traversed from left to right,
-- performing an action on each element.
-----------------------------------------------------------------------------

module Data.Traversable (
    -- * The 'Traversable' class
    Traversable(..),
    -- * Utility functions
    for,
    forM,
    mapAccumL,
    mapAccumR,
    -- * General definitions for superclass methods
    fmapDefault,
    foldMapDefault,

    -- * Overview
    -- $overview

    -- ** The 'traverse' and 'mapM' methods
    -- $traverse

    -- ** Validation use-case
    -- $validation

    -- ** The 'sequenceA' and 'sequence' methods
    -- $sequence

    -- ** Sample instance
    -- $sample_instance

    -- ** Construction
    --
    -- $construction

    -- * Laws
    --
    -- $laws

    -- * See also
    -- $also
    ) where

-- It is convenient to use 'Const' here but this means we must
-- define a few instances here which really belong in Control.Applicative
import Control.Applicative ( Const(..), ZipList(..) )
import Data.Coerce
import Data.Either ( Either(..) )
import Data.Foldable
import Data.Functor
import Data.Functor.Identity ( Identity(..) )
import Data.Functor.Utils ( StateL(..), StateR(..) )
import Data.Monoid ( Dual(..), Sum(..), Product(..),
                     First(..), Last(..), Alt(..), Ap(..) )
import Data.Ord ( Down(..) )
import Data.Proxy ( Proxy(..) )

import GHC.Arr
import GHC.Base ( Applicative(..), Monad(..), Monoid, Maybe(..), NonEmpty(..),
                  ($), (.), id, flip )
import GHC.Generics
import qualified GHC.List as List ( foldr )

-- | Functors representing data structures that can be traversed from
-- left to right.
--
class (Functor t, Foldable t) => Traversable t where
    {-# MINIMAL traverse | sequenceA #-}

    -- | Map each element of a structure to an action, evaluate these actions
    -- from left to right, and collect the results. For a version that ignores
    -- the results see 'Data.Foldable.traverse_'.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- In the first two examples we show each evaluated action mapping to the
    -- output structure.
    --
    -- >>> traverse Just [1,2,3,4]
    -- Just [1,2,3,4]
    --
    -- >>> traverse id [Right 1, Right 2, Right 3, Right 4]
    -- Right [1,2,3,4]
    --
    -- In the next examples, we show that 'Nothing' and 'Left' values short
    -- circuit the created structure.
    --
    -- >>> traverse (const Nothing) [1,2,3,4]
    -- Nothing
    --
    -- >>> traverse (\x -> if odd x then Just x else Nothing)  [1,2,3,4]
    -- Nothing
    --
    -- >>> traverse id [Right 1, Right 2, Right 3, Right 4, Left 0]
    -- Left 0
    --
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    {-# INLINE traverse #-}  -- See Note [Inline default methods]
    traverse f = sequenceA . fmap f

    -- | Evaluate each action in the structure from left to right, and
    -- collect the results. For a version that ignores the results
    -- see 'Data.Foldable.sequenceA_'.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- For the first two examples we show sequenceA fully evaluating a
    -- a structure and collecting the results.
    --
    -- >>> sequenceA [Just 1, Just 2, Just 3]
    -- Just [1,2,3]
    --
    -- >>> sequenceA [Right 1, Right 2, Right 3]
    -- Right [1,2,3]
    --
    -- The next two example show 'Nothing' and 'Just' will short circuit
    -- the resulting structure if present in the input. For more context,
    -- check the 'Traversable' instances for 'Either' and 'Maybe'.
    --
    -- >>> sequenceA [Just 1, Just 2, Just 3, Nothing]
    -- Nothing
    --
    -- >>> sequenceA [Right 1, Right 2, Right 3, Left 4]
    -- Left 4
    --
    sequenceA :: Applicative f => t (f a) -> f (t a)
    {-# INLINE sequenceA #-}  -- See Note [Inline default methods]
    sequenceA = traverse id

    -- | Map each element of a structure to a monadic action, evaluate
    -- these actions from left to right, and collect the results. For
    -- a version that ignores the results see 'Data.Foldable.mapM_'.
    --
    -- ==== __Examples__
    --
    -- 'mapM' is 'traverse' for 'Monad', and the following example shows
    -- how 'mapM' can apply an 'IO' action to a 'List' to produce a
    -- structured result.
    --
    -- Basic usage:
    --
    -- >>> import System.IO
    -- >>> mapM (openTempFile ".") ["t1", "t2"]
    -- [("./t169980-3",{handle: ./t169980-3}),("./t269980-4",{handle: ./t269980-4})]
    --
    mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    {-# INLINE mapM #-}  -- See Note [Inline default methods]
    mapM = traverse

    -- | Evaluate each monadic action in the structure from left to
    -- right, and collect the results. For a version that ignores the
    -- results see 'Data.Foldable.sequence_'.
    --
    -- ==== __Examples__
    --
    -- Basic usage:
    --
    -- The first two examples are instances where the input and
    -- and output of 'sequence' are isomorphic.
    --
    -- >>> sequence $ Right [1,2,3,4]
    -- [Right 1,Right 2,Right 3,Right 4]
    --
    -- >>> sequence $ [Right 1,Right 2,Right 3,Right 4]
    -- Right [1,2,3,4]
    --
    -- The following examples demonstrate short circuit behavior
    -- for 'sequence'.
    --
    -- >>> sequence $ Left [1,2,3,4]
    -- Left [1,2,3,4]
    --
    -- >>> sequence $ [Left 0, Right 1,Right 2,Right 3,Right 4]
    -- Left 0
    --
    sequence :: Monad m => t (m a) -> m (t a)
    {-# INLINE sequence #-}  -- See Note [Inline default methods]
    sequence = sequenceA

{- Note [Inline default methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

   class ... => Traversable t where
       ...
       mapM :: Monad m => (a -> m b) -> t a -> m (t b)
       mapM = traverse   -- Default method

   instance Traversable [] where
       {-# INLINE traverse #-}
       traverse = ...code for traverse on lists ...

This gives rise to a list-instance of mapM looking like this

  $fTraversable[]_$ctraverse = ...code for traverse on lists...
       {-# INLINE $fTraversable[]_$ctraverse #-}
  $fTraversable[]_$cmapM    = $fTraversable[]_$ctraverse

Now the $ctraverse obediently inlines into the RHS of $cmapM, /but/
that's all!  We get

  $fTraversable[]_$cmapM = ...code for traverse on lists...

with NO INLINE pragma!  This happens even though 'traverse' had an
INLINE pragma because the author knew it should be inlined pretty
vigorously.

Indeed, it turned out that the rhs of $cmapM was just too big to
inline, so all uses of mapM on lists used a terribly inefficient
dictionary-passing style, because of its 'Monad m =>' type.  Disaster!

Solution: add an INLINE pragma on the default method:

   class ... => Traversable t where
       ...
       mapM :: Monad m => (a -> m b) -> t a -> m (t b)
       {-# INLINE mapM #-}     -- VERY IMPORTANT!
       mapM = traverse
-}

-- instances for Prelude types

-- | @since 2.01
instance Traversable Maybe where
    traverse _ Nothing = pure Nothing
    traverse f (Just x) = Just <$> f x

-- | @since 2.01
instance Traversable [] where
    {-# INLINE traverse #-} -- so that traverse can fuse
    traverse f = List.foldr cons_f (pure [])
      where cons_f x ys = liftA2 (:) (f x) ys

-- | @since 4.9.0.0
instance Traversable NonEmpty where
  traverse f ~(a :| as) = liftA2 (:|) (f a) (traverse f as)

-- | @since 4.7.0.0
instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y

-- | @since 4.7.0.0
instance Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y

-- | @since 2.01
instance Ix i => Traversable (Array i) where
    traverse f arr = listArray (bounds arr) `fmap` traverse f (elems arr)

-- | @since 4.7.0.0
instance Traversable Proxy where
    traverse _ _ = pure Proxy
    {-# INLINE traverse #-}
    sequenceA _ = pure Proxy
    {-# INLINE sequenceA #-}
    mapM _ _ = pure Proxy
    {-# INLINE mapM #-}
    sequence _ = pure Proxy
    {-# INLINE sequence #-}

-- | @since 4.7.0.0
instance Traversable (Const m) where
    traverse _ (Const m) = pure $ Const m

-- | @since 4.8.0.0
instance Traversable Dual where
    traverse f (Dual x) = Dual <$> f x

-- | @since 4.8.0.0
instance Traversable Sum where
    traverse f (Sum x) = Sum <$> f x

-- | @since 4.8.0.0
instance Traversable Product where
    traverse f (Product x) = Product <$> f x

-- | @since 4.8.0.0
instance Traversable First where
    traverse f (First x) = First <$> traverse f x

-- | @since 4.8.0.0
instance Traversable Last where
    traverse f (Last x) = Last <$> traverse f x

-- | @since 4.12.0.0
instance (Traversable f) => Traversable (Alt f) where
    traverse f (Alt x) = Alt <$> traverse f x

-- | @since 4.12.0.0
instance (Traversable f) => Traversable (Ap f) where
    traverse f (Ap x) = Ap <$> traverse f x

-- | @since 4.9.0.0
instance Traversable ZipList where
    traverse f (ZipList x) = ZipList <$> traverse f x

-- | @since 4.9.0.0
deriving instance Traversable Identity


-- Instances for GHC.Generics
-- | @since 4.9.0.0
instance Traversable U1 where
    traverse _ _ = pure U1
    {-# INLINE traverse #-}
    sequenceA _ = pure U1
    {-# INLINE sequenceA #-}
    mapM _ _ = pure U1
    {-# INLINE mapM #-}
    sequence _ = pure U1
    {-# INLINE sequence #-}

-- | @since 4.9.0.0
deriving instance Traversable V1

-- | @since 4.9.0.0
deriving instance Traversable Par1

-- | @since 4.9.0.0
deriving instance Traversable f => Traversable (Rec1 f)

-- | @since 4.9.0.0
deriving instance Traversable (K1 i c)

-- | @since 4.9.0.0
deriving instance Traversable f => Traversable (M1 i c f)

-- | @since 4.9.0.0
deriving instance (Traversable f, Traversable g) => Traversable (f :+: g)

-- | @since 4.9.0.0
deriving instance (Traversable f, Traversable g) => Traversable (f :*: g)

-- | @since 4.9.0.0
deriving instance (Traversable f, Traversable g) => Traversable (f :.: g)

-- | @since 4.9.0.0
deriving instance Traversable UAddr

-- | @since 4.9.0.0
deriving instance Traversable UChar

-- | @since 4.9.0.0
deriving instance Traversable UDouble

-- | @since 4.9.0.0
deriving instance Traversable UFloat

-- | @since 4.9.0.0
deriving instance Traversable UInt

-- | @since 4.9.0.0
deriving instance Traversable UWord

-- Instance for Data.Ord
-- | @since 4.12.0.0
deriving instance Traversable Down

-- general functions

-- | 'for' is 'traverse' with its arguments flipped. For a version
-- that ignores the results see 'Data.Foldable.for_'.
for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
{-# INLINE for #-}
for = flip traverse

-- | 'forM' is 'mapM' with its arguments flipped. For a version that
-- ignores the results see 'Data.Foldable.forM_'.
forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
{-# INLINE forM #-}
forM = flip mapM

-- |The 'mapAccumL' function behaves like a combination of 'fmap'
-- and 'Data.Foldable.foldl'; it applies a function to each element of a structure,
-- passing an accumulating parameter from left to right, and returning
-- a final value of this accumulator together with the new structure.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> mapAccumL (\a b -> (a + b, a)) 0 [1..10]
-- (55,[0,1,3,6,10,15,21,28,36,45])
--
-- >>> mapAccumL (\a b -> (a <> show b, a)) "0" [1..5]
-- ("012345",["0","01","012","0123","01234"])
--
mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL f s t = runStateL (traverse (StateL . flip f) t) s

-- |The 'mapAccumR' function behaves like a combination of 'fmap'
-- and 'Data.Foldable.foldr'; it applies a function to each element of a structure,
-- passing an accumulating parameter from right to left, and returning
-- a final value of this accumulator together with the new structure.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> mapAccumR (\a b -> (a + b, a)) 0 [1..10]
-- (55,[54,52,49,45,40,34,27,19,10,0])
--
-- >>> mapAccumR (\a b -> (a <> show b, a)) "0" [1..5]
-- ("054321",["05432","0543","054","05","0"])
--
mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumR f s t = runStateR (traverse (StateR . flip f) t) s

-- | This function may be used as a value for `fmap` in a `Functor`
--   instance, provided that 'traverse' is defined. (Using
--   `fmapDefault` with a `Traversable` instance defined only by
--   'sequenceA' will result in infinite recursion.)
--
-- @
-- 'fmapDefault' f ≡ 'runIdentity' . 'traverse' ('Identity' . f)
-- @
fmapDefault :: forall t a b . Traversable t
            => (a -> b) -> t a -> t b
{-# INLINE fmapDefault #-}
-- See Note [Function coercion] in Data.Functor.Utils.
fmapDefault = coerce (traverse :: (a -> Identity b) -> t a -> Identity (t b))

-- | This function may be used as a value for `Data.Foldable.foldMap`
-- in a `Foldable` instance.
--
-- @
-- 'foldMapDefault' f ≡ 'getConst' . 'traverse' ('Const' . f)
-- @
foldMapDefault :: forall t m a . (Traversable t, Monoid m)
               => (a -> m) -> t a -> m
{-# INLINE foldMapDefault #-}
-- See Note [Function coercion] in Data.Functor.Utils.
foldMapDefault = coerce (traverse :: (a -> Const m ()) -> t a -> Const m (t ()))

------------------

-- $overview
-- @Traversable@ functors can be thought of as polymorphic containers that
-- support mapping of applicative (or monadic) effects over the container
-- (element-wise) to create a new container of __the same shape__, with the
-- effects sequenced in a natural order for the container type in question.
--
-- The 'Functor' base class means that the container cannot impose any
-- constraints on the element type, so containers that require elements to
-- be comparable, or hashable, etc., cannot be instances of the @Traversable@
-- class.

------------------

-- $traverse
-- For an 'Applicative' functor __@f@__ and a Traversable functor __@t@__, the
-- type signatures of 'traverse' and 'fmap' are rather similar:
--
-- > fmap     :: (a -> f b) -> t a -> t (f b)
-- > traverse :: (a -> f b) -> t a -> f (t b)
--
-- with one crucial difference: 'fmap' produces a container of effects, while
-- traverse produces an aggregate container-valued effect.  For example, when
-- __@f@__ is the __@IO@__ monad, and __@t@__ is the List functor, while 'fmap'
-- returns a list of pending IO actions 'traverse' returns an IO action that
-- evaluates to a list of the return values of the individual actions performed
-- left-to-right.
--
-- More concretely, if @nameAndLineCount@ counts the number of lines in a file,
-- returning a pair with input filename and the line count, then traversal
-- over a list of file names produces an IO action that evaluates to a list
-- of __@(fileName, lineCount)@__ pairs:
--
-- >>> nameAndLineCount :: FilePath -> IO (FilePath, Int)
-- >>> nameAndLineCount fn = ...
-- >>> traverse nameAndLineCount ["/etc/passwd","/etc/hosts"]
-- [("/etc/passwd",56),("/etc/hosts",32)]
--
-- The specialisation of 'traverse' to the case when __@f@__ is a monad is
-- called 'mapM'.  The two are otherwise generally identical:
--
-- > traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
-- > mapM     :: (Monad       m, Traversable t) => (a -> m b) -> t a -> m (t b)
--
-- The behaviour of 'traverse' and 'mapM' can be at first surprising when the
-- applicative functor __@f@__ is __@[]@__ (i.e. the List monad).  The List
-- monad is said to be /non-deterministic/, by which is meant that applying a
-- list of __@n@__ functions __@[a -> b]@__ to a list of __@k@__ values
-- __@[a]@__ produces a list of __@n*k@__ values of each function applied to
-- each input value.
--
-- As a result, traversal with a function __@f :: a -> [b]@__, over an input
-- container __@t a@__, yields a list __@[t b]@__, whose length is the product
-- of the lengths of the lists that the function returns for each element of
-- the input container!  The individual elements __@a@__ of the container are
-- replaced by each element of __@f a@__ in turn:
--
-- >>> mapM (\n -> [0..n]) $ Just 2
-- [Just 0, Just 1, Just 2]
-- >>> mapM (\n -> [0..n]) [0..2]
-- [[0,0,0],[0,0,1],[0,0,2],[0,1,0],[0,1,1],[0,1,2]]
--
-- If any element of the container is mapped to an empty list, then the
-- aggregate result is empty (no value is available to fill one of the
-- slots of the output container).
--
-- >>> traverse (const []) $ Just 0
-- []
--
-- When however the traversed container is empty, the result is always a
-- singleton of the empty container, the function is never evaluated
-- as there are no input values for it to be applied to.
--
-- >>> traverse (const []) Nothing
-- [Nothing]
--
-- The result of 'traverse' is all-or-nothing, either containers of exactly the
-- same shape as the input or a failure ('Nothing', 'Left', empty list, etc.).
-- The 'traverse' function does not perform selective filtering as with e.g.
-- 'Data.Maybe.mapMaybe':
--
-- >>> let incOdd n = if odd n then Just $ n + 1 else Nothing
-- >>> traverse incOdd [1, 2, 3]
-- Nothing
-- >>> mapMaybe incOdd [1, 2, 3]
-- [2,4]

------------------

-- $validation
-- A hypothetical application of the above is to validate a structure:
--
-- >>> validate :: Int -> Either (String, Int) Int
-- >>> validate i = if odd i then Left ("That's odd", i) else Right i
-- >>> traverse validate [2,4,6,8,10]
-- Right [2,4,6,8,10]
-- >>> traverse validate [2,4,6,8,9]
-- Left ("That's odd",9)
--
-- >>> -- Since 'Nothing' is an empty structure, none of its elements are odd.
-- >>> traverse validate Nothing
-- Right Nothing
-- >>> traverse validate (Just 42)
-- Right (Just 42)
-- >>> traverse validate (Just 17)
-- Left ("That's odd",17)
--
-- However, this is not terribly efficient, because we pay the cost of
-- reconstructing the entire structure as a side effect of validation.
-- It is generally cheaper to just check all the elements and then use
-- the original structure if it is valid.  This can be done via the
-- methods of the 'Foldable' superclass, which perform only the
-- side effects without generating a new structure:
--
-- >>> traverse_ validate [2,4,6,8,10]
-- Right ()
-- >>> traverse_ validate [2,4,6,8,9]
-- Left ("That's odd",9)
--
-- The @Foldable@ instance should be defined in a manner that avoids
-- construction of an unnecesary copy of the container.
--
-- Perhaps the most widely used @Foldable@ methods are 'mapM_' and its flipped
-- version 'forM_'.  Often, to sequence IO actions (that return no useful
-- results) over all the elements of a @Traversable@ container.  One special
-- case is a 'Maybe' container that optionally holds a value. Given:
--
-- > action :: a -> IO ()
-- > mvalue :: Maybe a
--
-- if you want to evaluate the __@action@__ in the @Just@ case, and do
-- nothing otherwise, you can write the more concise and more general:
--
-- > mapM_ action mvalue
--
-- rather than
--
-- > maybe (return ()) action mvalue
--
-- The 'mapM_' form works verbatim if the type of __@mvalue@__ is later
-- refactored from __@Maybe a@__ to __@Either e a@__ (assuming it remains
-- OK to silently do nothing in the error case).
--
-- There's even a generic way to handle empty values ('Nothing', 'Left', etc.):
--
-- > case traverse_ (const Nothing) mvalue of
-- >     Nothing -> mapM_ action mvalue -- mvalue is non-empty
-- >     Just () -> ... handle empty mvalue ...

------------------

-- $sequence
-- The 'sequenceA' and 'sequence' methods are useful when what you have is a
-- container of applicative or, respectively, monadic actions, and you want to
-- evaluate them left-to-right to obtain a container of the computed values.
--
-- > sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
-- > sequence  :: (Monad       m, Traversable t) => t (m a) -> m (t a)
-- > sequenceA = traverse id
-- > sequence  = mapM id
--
-- When the monad __@m@__ is 'System.IO.IO', applying 'sequence' to a list of
-- IO actions, performs each in turn, returning a list of the results:
--
-- > sequence [putStr "Hello ", putStrLn "World!"]
-- >     = (\a b -> [a,b]) <$> putStr "Hello " <*> putStrLn "World!"
-- >     = do u1 <- putStr "Hello "
-- >          u2 <- putStrLn "World!"
-- >          return (u1, u2)
--
-- For 'sequenceA', the /non-deterministic/ behaviour of @List@ is most easily
-- seen in the case of a list of lists (of elements of some common fixed type).
-- The result is a cross-product of all the sublists:
--
-- >>> sequenceA [[0, 1, 2], [30, 40], [500]]
-- [[0,30,500],[0,40,500],[1,30,500],[1,40,500],[2,30,500],[2,40,500]]
--
-- When the monad __@m@__ is 'Maybe' or 'Either', the effect in question is to
-- short-circuit the computation on encountering 'Nothing' or 'Left'.
--
-- >>> sequence [Just 1,Just 2,Just 3]
-- Just [1,2,3]
-- >>> sequence [Just 1,Nothing,Just 3]
-- Nothing
-- >>> sequence [Right 1,Right 2,Right 3]
-- Right [1,2,3]
-- >>> sequence [Right 1,Left "sorry",Right 3]
-- Left "sorry"
--
-- The result of 'sequence' is all-or-nothing, either containers of exactly the
-- same shape as the input or a failure ('Nothing', 'Left', empty list, etc.).
-- The 'sequence' function does not perform selective filtering as with e.g.
-- 'Data.Maybe.catMaybes' or 'Data.Either.rights':
--
-- >>> catMaybes [Just 1,Nothing,Just 3]
-- [1,3]
-- >>> rights [Right 1,Left "sorry",Right 3]
-- [1,3]

------------------

-- $sample_instance
--
-- Instances are similar to 'Functor', e.g. given a data type
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a suitable instance would be
--
-- > instance Traversable Tree where
-- >    traverse f Empty = pure Empty
-- >    traverse f (Leaf x) = Leaf <$> f x
-- >    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
--
-- This definition works for any applicative functor in the co-domain of @f@,
-- as the laws for '<*>' imply a form of associativity.

------------------

-- $construction
--
-- How do @Traversable@ functors manage to construct a new container of the
-- same shape by sequencing effects over their elements?  Well, left-to-right
-- traversal with sequencing of effects suggests induction from a base case, so
-- the first question is what is the base case?  A @Traversable@ container with
-- elements of type __@a@__ generally has some minimal form that is either
-- "empty" or has just a single element (think "Data.List" vs.
-- "Data.List.Nonempty").
--
-- * If the base case is empty (no associated first value of __@a@__) then
--   traversal just reproduces the empty structure with no side effects,
--   so we have:
--
--     > traverse _ empty = pure empty
--
--     With the List monad, "empty" is __@[]@__, while with 'Maybe' it is
--     'Nothing'.  With __@Either e a@__ we have an /empty/ case for each
--     value of __@e@__.
--
-- * If the base case is a __@singleton a@__, then 'traverse' can take that
--   __@a@__, apply __@f :: a -> F b@__ getting an __@F b@__, then
--   __@fmap singleton@__ over that, getting __@F (singleton b)@__:
--
--     > traverse f (singleton a) = singleton <$> f a
--
-- Since 'Maybe' and 'Either' are either empty or singletons, we have
--
-- > traverse _ Nothing = pure Nothing
-- > traverse f (Just a) = Just <$> f a
--
-- > traverse _ (Left e) = pure (Left e)
-- > traverse f (Right a) = Right <$> f a
--
-- Similarly, for List, we have:
--
-- > traverse f [] = pure []
-- > traverse f [a] = fmap (:[]) (f a) = (:) <$> f a <*> pure []
--
-- What remains to be done is an inductive step beyond the empty and singleton
-- cases.  For a concrete @Traversable@ functor @T@ we need to be able to
-- extend our structure incrementally by filling in holes.  We can view a
-- partially built structure __@t0 :: T a@__ as a function
-- __@append :: a -> T a@__ that takes one more element __@a@__ to insert into
-- the container to the right of the existing elements to produce a larger
-- structure.  Conversely, we can view an element @a@ as a function
-- __@prepend :: T a -> T a@__ of a partially built structure that inserts the
-- element to the left of the existing elements.
--
-- Assuming that 'traverse' has already been defined on the partially built
-- structure:
--
-- > f0 = traverse f t0 :: F (T b)
--
-- we aim to define __@traverse f (append t0 a)@__ and/or
-- __@traverse f (prepend a t0)@__.
--
-- We can lift @append@ and apply it to @f0@ to get:
--
-- > append <$> f0 :: F (b -> T b)
--
-- and from the /next/ element __@a@__ we can obtain __@f a :: F b@__, and
-- this is where we'll make use of the applicative instance of @F@.  Adding
-- one more element on the right is then:
--
-- > traverse f (append t0 a) = append <$> traverse f t0 <*> f a
--
-- while prepending an element on the left is:
--
-- > traverse f (prepend a t0) = prepend <$> f a <*> traverse f t0
--
-- The (binary) @Tree@ instance example makes use of both, after defining the
-- @Empty@ base case and the singleton @Leaf@ node case, non-empty internal
-- nodes introduce both a prepended child node on the left and an appended
-- child node on the right:
--
-- > traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
--
-- The above definitions sequence the 'Applicative' effects of __@F@__ in the
-- expected order while producing results of the expected shape __@T@__.
--
-- For lists we get the natural order of effects by using
-- __@(prepend \<$\> f a)@__ as the operator and __@(traverse f as)@__ as the
-- operand (the actual definition is written as an equivalent right fold
-- in order to enable /fusion/ rules):
--
-- > traverse f [] = pure []
-- > traverse f (a:as) = (:) <$> f a <*> traverse f as
--
-- The origin of the combinatorial product when __@F@__ is __@[]@__ should now
-- be apparent, the /non-deterministic/ definition of @\<*\>@ for @List@ makes
-- multiple independent choices for each element of the structure.

------------------

-- $laws
-- A definition of 'traverse' must satisfy the following laws:
--
-- [Naturality]
--   @t . 'traverse' f = 'traverse' (t . f)@
--   for every applicative transformation @t@
--
-- [Identity]
--   @'traverse' 'Identity' = 'Identity'@
--
-- [Composition]
--   @'traverse' ('Data.Functor.Compose.Compose' . 'fmap' g . f)
--     = 'Data.Functor.Compose.Compose' . 'fmap' ('traverse' g) . 'traverse' f@
--
-- A definition of 'sequenceA' must satisfy the following laws:
--
-- [Naturality]
--   @t . 'sequenceA' = 'sequenceA' . 'fmap' t@
--   for every applicative transformation @t@
--
-- [Identity]
--   @'sequenceA' . 'fmap' 'Identity' = 'Identity'@
--
-- [Composition]
--   @'sequenceA' . 'fmap' 'Data.Functor.Compose.Compose'
--     = 'Data.Functor.Compose.Compose' . 'fmap' 'sequenceA' . 'sequenceA'@
--
-- where an /applicative transformation/ is a function
--
-- @t :: (Applicative f, Applicative g) => f a -> g a@
--
-- preserving the 'Applicative' operations, i.e.
--
-- @
-- t ('pure' x) = 'pure' x
-- t (f '<*>' x) = t f '<*>' t x
-- @
--
-- and the identity functor 'Identity' and composition functors
-- 'Data.Functor.Compose.Compose' are from "Data.Functor.Identity" and
-- "Data.Functor.Compose".
--
-- A result of the naturality law is a purity law for 'traverse'
--
-- @'traverse' 'pure' = 'pure'@
--
-- (The naturality law is implied by parametricity and thus so is the
-- purity law [1, p15].)
--
-- The superclass instances should satisfy the following:
--
--  * In the 'Functor' instance, 'fmap' should be equivalent to traversal
--    with the identity applicative functor ('fmapDefault').
--
--  * In the 'Foldable' instance, 'Data.Foldable.foldMap' should be
--    equivalent to traversal with a constant applicative functor
--    ('foldMapDefault').

------------------

-- $also
--
--  * [1] \"The Essence of the Iterator Pattern\",
--    by Jeremy Gibbons and Bruno Oliveira,
--    in /Mathematically-Structured Functional Programming/, 2006, online at
--    <http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/#iterator>.
--
--  * \"Applicative Programming with Effects\",
--    by Conor McBride and Ross Paterson,
--    /Journal of Functional Programming/ 18:1 (2008) 1-13, online at
--    <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>.
--
--  * \"An Investigation of the Laws of Traversals\",
--    by Mauro Jaskelioff and Ondrej Rypacek,
--    in /Mathematically-Structured Functional Programming/, 2012, online at
--    <http://arxiv.org/pdf/1202.2919>.
