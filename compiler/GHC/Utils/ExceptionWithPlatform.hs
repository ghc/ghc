{-# LANGUAGE GADTs #-}

module GHC.Utils.ExceptionWithPlatform
   ( -- * Exceptions requiring 'Platform'
     ExceptionWithPlatform(..)
   , handleWithPlatform
   , rethrowWithPlatform
   , throwWithPlatform
   ) where

import GHC.Prelude
import GHC.Platform

import Control.Monad.Catch
import Control.Exception (throw)
import Data.Typeable      ( cast, typeRep )

-- | An exception which requires access to a 'Platform' to produce.
data ExceptionWithPlatform where
    ExceptionWithPlatform :: forall a. (Exception a)
                          => (Platform -> a) -> ExceptionWithPlatform

instance Show ExceptionWithPlatform where
    show (ExceptionWithPlatform (_ :: Platform -> a)) =
        "ExceptionWithPlatform @("++show ty++") _"
      where
        ty = typeRep ([] :: [a])

instance Exception ExceptionWithPlatform

handleWithPlatform
    :: forall m a r. (Exception a, MonadCatch m)
    => Platform
    -> (a -> m r)
    -> m r
    -> m r
handleWithPlatform platform handler action =
    catchJust select action handler
  where
    select :: ExceptionWithPlatform -> Maybe a
    select (ExceptionWithPlatform e) = cast e

rethrowWithPlatform
    :: (MonadCatch m) => Platform -> m r -> m r
rethrowWithPlatform platform action =
    catch action (\(ExceptionWithPlatform f) -> throwM (f platform))

throwWithPlatform :: (Exception a) => (Platform -> a) -> r
throwWithPlatform f = throw (ExceptionWithPlatform f)

