{-# language DataKinds #-}

module ListTuplePunsFail5 where

f ::
  Monad m =>
  (Monad m, (Monad m, Monad m)) =>
  m Int
f = pure 5
