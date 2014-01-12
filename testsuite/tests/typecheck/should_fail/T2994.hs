{-# LANGUAGE MultiParamTypeClasses #-}

-- Trac #2994

module T2994 where

class MonadReader a b 

newtype Reader' r a = Reader' (r -> a)

instance MonadReader Int

instance MonadReader (Reader' r)

instance MonadReader r r (Reader' r)
