{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative.Lift
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Adding a new kind of pure computation to an applicative functor.
-----------------------------------------------------------------------------

module Control.Applicative.Lift (
    -- * Lifting an applicative
    Lift(..),
    unLift,
    mapLift,
    elimLift,
    -- * Collecting errors
    Errors,
    runErrors,
    failure,
    eitherToErrors
  ) where

import Data.Functor.Classes

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Functor.Constant
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(traverse))

-- | Applicative functor formed by adding pure computations to a given
-- applicative functor.
data Lift f a = Pure a | Other (f a)

instance (Eq1 f) => Eq1 (Lift f) where
    liftEq eq (Pure x1) (Pure x2) = eq x1 x2
    liftEq _ (Pure _) (Other _) = False
    liftEq _ (Other _) (Pure _) = False
    liftEq eq (Other y1) (Other y2) = liftEq eq y1 y2
    {-# INLINE liftEq #-}

instance (Ord1 f) => Ord1 (Lift f) where
    liftCompare comp (Pure x1) (Pure x2) = comp x1 x2
    liftCompare _ (Pure _) (Other _) = LT
    liftCompare _ (Other _) (Pure _) = GT
    liftCompare comp (Other y1) (Other y2) = liftCompare comp y1 y2
    {-# INLINE liftCompare #-}

instance (Read1 f) => Read1 (Lift f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith rp "Pure" Pure `mappend`
        readsUnaryWith (liftReadsPrec rp rl) "Other" Other

instance (Show1 f) => Show1 (Lift f) where
    liftShowsPrec sp _ d (Pure x) = showsUnaryWith sp "Pure" d x
    liftShowsPrec sp sl d (Other y) =
        showsUnaryWith (liftShowsPrec sp sl) "Other" d y

instance (Eq1 f, Eq a) => Eq (Lift f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (Lift f a) where compare = compare1
instance (Read1 f, Read a) => Read (Lift f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (Lift f a) where showsPrec = showsPrec1

instance (Functor f) => Functor (Lift f) where
    fmap f (Pure x) = Pure (f x)
    fmap f (Other y) = Other (fmap f y)
    {-# INLINE fmap #-}

instance (Foldable f) => Foldable (Lift f) where
    foldMap f (Pure x) = f x
    foldMap f (Other y) = foldMap f y
    {-# INLINE foldMap #-}

instance (Traversable f) => Traversable (Lift f) where
    traverse f (Pure x) = Pure <$> f x
    traverse f (Other y) = Other <$> traverse f y
    {-# INLINE traverse #-}

-- | A combination is 'Pure' only if both parts are.
instance (Applicative f) => Applicative (Lift f) where
    pure = Pure
    {-# INLINE pure #-}
    Pure f <*> Pure x = Pure (f x)
    Pure f <*> Other y = Other (f <$> y)
    Other f <*> Pure x = Other (($ x) <$> f)
    Other f <*> Other y = Other (f <*> y)
    {-# INLINE (<*>) #-}

-- | A combination is 'Pure' only either part is.
instance (Alternative f) => Alternative (Lift f) where
    empty = Other empty
    {-# INLINE empty #-}
    Pure x <|> _ = Pure x
    Other _ <|> Pure y = Pure y
    Other x <|> Other y = Other (x <|> y)
    {-# INLINE (<|>) #-}

-- | Projection to the other functor.
unLift :: (Applicative f) => Lift f a -> f a
unLift (Pure x) = pure x
unLift (Other e) = e
{-# INLINE unLift #-}

-- | Apply a transformation to the other computation.
mapLift :: (f a -> g a) -> Lift f a -> Lift g a
mapLift _ (Pure x) = Pure x
mapLift f (Other e) = Other (f e)
{-# INLINE mapLift #-}

-- | Eliminator for 'Lift'.
--
-- * @'elimLift' f g . 'pure' = f@
--
-- * @'elimLift' f g . 'Other' = g@
--
elimLift :: (a -> r) -> (f a -> r) -> Lift f a -> r
elimLift f _ (Pure x) = f x
elimLift _ g (Other e) = g e
{-# INLINE elimLift #-}

-- | An applicative functor that collects a monoid (e.g. lists) of errors.
-- A sequence of computations fails if any of its components do, but
-- unlike monads made with 'ExceptT' from "Control.Monad.Trans.Except",
-- these computations continue after an error, collecting all the errors.
--
-- * @'pure' f '<*>' 'pure' x = 'pure' (f x)@
--
-- * @'pure' f '<*>' 'failure' e = 'failure' e@
--
-- * @'failure' e '<*>' 'pure' x = 'failure' e@
--
-- * @'failure' e1 '<*>' 'failure' e2 = 'failure' (e1 '<>' e2)@
--
type Errors e = Lift (Constant e)

-- | Extractor for computations with accumulating errors.
--
-- * @'runErrors' ('pure' x) = 'Right' x@
--
-- * @'runErrors' ('failure' e) = 'Left' e@
--
runErrors :: Errors e a -> Either e a
runErrors (Other (Constant e)) = Left e
runErrors (Pure x) = Right x
{-# INLINE runErrors #-}

-- | Report an error.
failure :: e -> Errors e a
failure e = Other (Constant e)
{-# INLINE failure #-}

-- | Convert from 'Either' to 'Errors' (inverse of 'runErrors').
eitherToErrors :: Either e a -> Errors e a
eitherToErrors = either failure Pure
