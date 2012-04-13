{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module T5771 where

class IndexedMonad m where
  unit :: a -> m i i a
  bind :: m i j a -> (a -> m j k b) -> m i k b

newtype IndexedIO i j a = IndexedIO {runIndexedIO :: IO a}

-- i and j are both *; instance is accepted
instance IndexedMonad IndexedIO where
  unit = IndexedIO . return
  bind m k = IndexedIO $ runIndexedIO m >>= runIndexedIO . k
infixl 1 `bind`

data HList xs where
  N    :: HList '[]
  (:>) :: a -> HList as -> HList (a ': as)
infixr 5 :>

newtype HLState xs ys a = HLState {runHLState :: HList xs -> (a, HList ys)}

-- i and j are now [*]; rejected with the MPTCs message
instance IndexedMonad HLState where
  unit x = HLState $ \s -> (x, s)
  bind (HLState f) k = HLState $ \xs ->
    case f xs of (a, ys) -> runHLState (k a) ys
