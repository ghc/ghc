{-# LANGUAGE Haskell2010 #-}
module ReaderT where

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
