-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Fix
-- Copyright   :  (c) Andy Gill 2001,
--		  (c) Oregon Graduate Institute of Science and Technology, 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A class for monadic (value) recursion and its implementation.
-- For details:
-- Levent Erkök. Value recursion in Monadic Computations. 
-- Oregon Graduate Institute, OHSU. Portland, Oregon. October 2002.
-- http://www.cse.ogi.edu/~erkok/rmb/
-----------------------------------------------------------------------------

module Control.Monad.X.Fix (
	MonadFix(
	   mfix	-- :: (a -> m a) -> m a
         ),
	fix	-- :: (a -> a) -> a
  ) where

import Prelude
import System.IO
import Monad(liftM)

import Control.Monad.X.Trans
import Control.Monad.X.Identity
import Control.Monad.X.Types
import Control.Monad.X.ReaderT
import Control.Monad.X.WriterT
import Control.Monad.X.StateT
import Control.Monad.X.ErrorT
import Control.Monad.X.NondetT

fix :: (a -> a) -> a
fix f = let x = f x in x

class (Monad m) => MonadFix m where
  mfix :: (a -> m a) -> m a




instance MonadFix Maybe where
  mfix f  = let a = f (unJust a) in a
             where unJust (Just x) = x

instance MonadFix [] where
  mfix f  = case fix (f . head) of
              []    -> []
              (x:_) -> x : mfix (tail . f)

instance MonadFix IO where
  mfix    = fixIO 

instance MonadFix Identity where
  mfix f  = return (fix (runIdentity . f))

instance (MonadFix m) => MonadFix (ReaderT r m) where
  mfix f  = R (\r -> mfix (\a -> unR (f a) r))

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
  mfix m  = W (mfix (\ ~(a, _) -> unW (m a)))

instance (MonadFix m) => MonadFix (StateT s m) where
  mfix f  = S (\s -> mfix (\ ~(a, _) -> unS (f a) s))

instance (MonadFix m) => MonadFix (ErrorT e m) where
  mfix f  = E (mfix (unE . f . either (error "ErrorT: mfix looped") id))

-- is that right?
instance MonadFix m => MonadFix (NondetT m) where
  mfix f  = N (do x <- mfix (unN . f . hd)
                  case x of
                    Empty    -> return Empty
                    Cons a _ -> return (Cons a (mfix (tl . f))))
    where hd (Cons a _) = a
          hd _          = error "NondetT: mfix looped (hd)"
          tl m          = N (do x <- unN m
                                case x of
                                  Cons _ m -> unN m
                                  _ -> error "NondetT: mfix looped (tl)")
        

{-
instance MonadFix m => MonadFix (NondetT m) where
  mfix f  = Re (do x <- mfix (unRe . f . hd)
                  case x of
                    Value a  -> return (Value a)
                    Delay m  -> return (Delay (mfix (tl . f)))
    where hd (Value a)  = a
          hd _          = error "ResumeT: mfix looped (hd)"
          tl m          = Re (do x <- unRe m
                                case x of
                                  
                                  Cons _ m -> unN m
                                  _ -> error "NondetT: mfix looped (tl)")
-}

