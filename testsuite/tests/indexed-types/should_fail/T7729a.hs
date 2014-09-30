{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module T7729a where
import Control.Monad

class Monad m => PrimMonad m where
  type PrimState m

class MonadTrans t where
  lift :: Monad m => m a -> t m a

class (PrimMonad (BasePrimMonad m), Monad m) => MonadPrim m where
  type BasePrimMonad m :: * -> *
  liftPrim :: BasePrimMonad m a -> m a


newtype Rand m a = Rand {
  runRand :: Maybe (m ()) -> m a
  }

instance Monad m => Functor (Rand m) where
  fmap = liftM

instance Monad m => Applicative (Rand m) where
  pure  = return
  (<*>) = ap

instance (Monad m) => Monad (Rand m) where
  return           = Rand . const . return
  (Rand rnd) >>= f = Rand $ \g -> (\x -> runRand (f x) g) =<< rnd g

instance MonadTrans Rand where
  lift = Rand . const

instance MonadPrim m => MonadPrim (Rand m) where
  type BasePrimMonad (Rand m) = BasePrimMonad m
  liftPrim x = liftPrim (lift x)   -- This line changed from T7729

{-
  liftPrim :: (MonadPrim m) => BasePrimMonad m a -> m a
  lift :: MonadTrans t, Monad m => m a -> t m a

  sig of liftPrim :: BasePrimMonad (Rand m) a -> Rand m a
                   = BasePrimMonad m a -> Rand m a
  
  x :: BasePrimMonad (Rand m) a
  lift @ t=tt @ m=m1
  liftPrim @ m=m2 @ a=aa

  forall m. (Monad m) => BasePrimMonad (Rand m) a ~ m1 a   -- x arg of lift

                         tt m1 a    -- Result of lift
                               ~ 
                            BasePrimMonad m2 a   -- Arg of liftPrim

                         Rand m a      -- expected type of RHS
                             ~
                             m2 a  -- Result of liftPrim
    m = m_and
    m1 = m_aql
    m2 = m_aqf
    tt = t_aqj

---->
     m2 := Rand m

a)     BasePrimMonad (Rand m) ~ m1
b)     tt m1 ~ BasePrimMonad (Rand m)

--->  process (b) first
    CFunEqCan   BasePrimMonad (Ramd m) ~ s_atH
                s_atH ~ tt m1

  
--->  now process (a)
    m1 ~ s_atH ~ tt m1    -- An obscure occurs check
-}

