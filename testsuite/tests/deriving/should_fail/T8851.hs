{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module T8851 where

import Control.Applicative

class Parsing m where
  notFollowedBy :: (Monad m, Show a) => m a -> m ()

data Parser a
instance Parsing Parser where
  notFollowedBy = undefined

instance Functor Parser where
  fmap = undefined
instance Applicative Parser where
  pure = undefined
  (<*>) = undefined
instance Monad Parser where
  return = undefined
  (>>=) = undefined

newtype MyParser a = MkMP (Parser a)
  deriving Parsing