module T27005_aux (T(..), S(..), f) where
import GHC.Stack

class Monad m => C m
instance C IO

data S = MkS !(IO ()) Int
data T = MkT !S

{-# SPECIALISE f :: HasCallStack => T -> IO T #-}
f :: (C m, HasCallStack) => T -> m T
f (MkT a) = do () <- pure (); pure $! MkT a
