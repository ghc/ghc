{-# LANGUAGE ImpredicativeTypes #-}

module T11319 where

f :: Monad m => m (Maybe a)
f = return Nothing
