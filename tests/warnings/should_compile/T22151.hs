{-# LANGUAGE UndecidableInstances #-}
module T22151 where

import Control.Monad.IO.Class (MonadIO(liftIO))

class (Applicative m, Monad m) => C m where
  m :: m ()

-- This should not emit a -Wredundant-constraints warning. This is because
-- GHC should not expand the superclasses of the Given constraint `MonadIO m`
-- given that it is not Paterson-smaller than the instance head `C m`. (See
-- Note [Recursive superclasses] in GHC.Tc.TyCl.Instance for more on what
-- "Paterson-smaller" means.) As a result, we must provide the `Applicative m`
-- and `Monad m` constraints explicitly.
instance (Applicative m, Monad m, MonadIO m) => C m where
  m = liftIO (pure ())
