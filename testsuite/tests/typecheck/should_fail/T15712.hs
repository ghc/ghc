{-# Language RankNTypes    #-}
{-# Language DerivingVia   #-}
{-# Language DeriveFunctor #-}

-- import Control.Monad.Codensity
import Data.Kind

newtype Codensity f a = Codensity (forall xx. (a -> f xx) -> f xx)
  deriving
    (Functor)

newtype GEndo m a = GEndo (m a -> m a)

newtype LogicT m a = LogicT (forall xx. (a -> (m xx -> m xx)) -> (m xx -> m xx))
  deriving
    (Functor)
  via
    (Codensity GEndo)
