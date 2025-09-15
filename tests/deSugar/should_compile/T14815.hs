-- Desugarer outputs of this program when compiled with and without -XStrict
-- should be the same because this program has only one binder (`a` in function
-- `primitive`), but the binder is annotated with a laziness annotation, so
-- -XStrict should have no effect on that binder.
--
-- Derived methods are also effected by -XStrict, but in our case we derive via
-- GND which just generates coercions like
--
--     instance Functor m => Functor (StateT s m) where
--       fmap
--         = coerce
--             @(forall (a_aJ2 :: TYPE LiftedRep) (b_aJ3 :: TYPE LiftedRep).
--               a_aJ2 -> b_aJ3
--               -> StateT s_aDW m_aDX a_aJ2 -> StateT s_aDW m_aDX b_aJ3)
--             @(forall (a_aJ2 :: TYPE LiftedRep) (b_aJ3 :: TYPE LiftedRep).
--               a_aJ2 -> b_aJ3
--               -> StateT s_aDW m_aDX a_aJ2 -> StateT s_aDW m_aDX b_aJ3)
--             fmap
--
-- So really -XStrict shouldn't have any effect on this program.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}

module K where

import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans
import GHC.Exts

class Monad m => PrimMonad m where
  type PrimState m
  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a

newtype StateT s m a = StateT (S.StateT s m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

instance PrimMonad m => PrimMonad (StateT s m) where
  type PrimState (StateT s m) = PrimState m
  primitive ~a = lift (primitive a) ; {-# INLINE primitive #-}
