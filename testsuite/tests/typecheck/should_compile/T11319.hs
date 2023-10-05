{-# LANGUAGE ImpredicativeTypes #-}

-- Sept 16: this compiles again now, because I've weakened
--          ImpredicativeTypes a lot

module T11319 where

f :: Monad m => m (Maybe a)
f = return Nothing
