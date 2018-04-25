{-# OPTIONS_GHC -dcore-lint -fdefer-typed-holes #-}
module T12947 where

import qualified Control.Monad.Fail as Fail

newtype P m a = P { unP :: (a -> IO (m ())) -> IO (m ()) }

instance Functor (P m) where

instance Applicative (P m) where

instance Monad (P m) where

instance (Fail.MonadFail m) => Fail.MonadFail (P m) where
  fail msg = ContT $ \ _ -> Fail.fail msg
