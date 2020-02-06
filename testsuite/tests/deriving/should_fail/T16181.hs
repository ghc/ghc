{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
module T16181 where

import Control.Monad.Trans.Class
import Control.Monad.Reader
import Data.Functor.Const (Const(..))
import Data.Functor.Classes
import Data.Kind
import Data.Proxy

newtype FlipConst a b = FlipConst b
  deriving (Show1, Eq1) via (Const b)

data Foo m x = Foo { foo :: m x }
newtype Q x m a = Q {unQ :: Foo m x -> m a}
    deriving (Functor, Applicative, Monad, MonadReader (Foo m x)) via (ReaderT (Foo m x) m)
    deriving MonadTrans via (ReaderT (Foo m x))

class C (f :: Type -> Type) where
  m :: Proxy f -> String
instance C (Either a) where
  m _ = "Either"
data T a
  deriving C via Either a
