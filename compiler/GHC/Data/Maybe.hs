
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Data.Maybe (
        module Data.Maybe,

        MaybeErr(..), -- Instance of Monad
        failME, isSuccess,

        orElse,
        firstJust, firstJusts, firstJustsM,
        whenIsJust,
        expectJust,
        rightToMaybe,

        -- * MaybeT
        MaybeT(..), liftMaybeT, tryMaybeT
    ) where

import GHC.Prelude
import GHC.IO (catchException)

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Foldable ( foldlM )
import GHC.Utils.Misc (HasCallStack)

infixr 4 `orElse`

{-
************************************************************************
*                                                                      *
\subsection[Maybe type]{The @Maybe@ type}
*                                                                      *
************************************************************************
-}

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust a b = firstJusts [a, b]

-- | Takes a list of @Maybes@ and returns the first @Just@ if there is one, or
-- @Nothing@ otherwise.
firstJusts :: [Maybe a] -> Maybe a
firstJusts = msum

-- | Takes computations returnings @Maybes@; tries each one in order.
-- The first one to return a @Just@ wins. Returns @Nothing@ if all computations
-- return @Nothing@.
firstJustsM :: (Monad m, Foldable f) => f (m (Maybe a)) -> m (Maybe a)
firstJustsM = foldlM go Nothing where
  go :: Monad m => Maybe a -> m (Maybe a) -> m (Maybe a)
  go Nothing         action  = action
  go result@(Just _) _action = return result

expectJust :: HasCallStack => String -> Maybe a -> a
{-# INLINE expectJust #-}
expectJust _   (Just x) = x
expectJust err Nothing  = error ("expectJust " ++ err)

whenIsJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenIsJust (Just x) f = f x
whenIsJust Nothing  _ = return ()

-- | Flipped version of @fromMaybe@, useful for chaining.
orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right x) = Just x

{-
************************************************************************
*                                                                      *
\subsection[MaybeT type]{The @MaybeT@ monad transformer}
*                                                                      *
************************************************************************
-}

-- We had our own MaybeT in the past. Now we reuse transformer's MaybeT

liftMaybeT :: Monad m => m a -> MaybeT m a
liftMaybeT act = MaybeT $ Just `liftM` act

-- | Try performing an 'IO' action, failing on error.
tryMaybeT :: IO a -> MaybeT IO a
tryMaybeT action = MaybeT $ catchException (Just `fmap` action) handler
  where
    handler (SomeExceptionWithLocation _) = return Nothing

{-
************************************************************************
*                                                                      *
\subsection[MaybeErr type]{The @MaybeErr@ type}
*                                                                      *
************************************************************************
-}

data MaybeErr err val = Succeeded val | Failed err
    deriving (Functor)

instance Applicative (MaybeErr err) where
  pure  = Succeeded
  (<*>) = ap

instance Monad (MaybeErr err) where
  Succeeded v >>= k = k v
  Failed e    >>= _ = Failed e

isSuccess :: MaybeErr err val -> Bool
isSuccess (Succeeded {}) = True
isSuccess (Failed {})    = False

failME :: err -> MaybeErr err val
failME e = Failed e
