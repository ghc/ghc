{-# LANGUAGE RankNTypes #-}

module GHCBug (field) where

import MiniLens ((^.), Getting, Lens', lens, view)

t' :: Getting () () ()
t' = lens id const
{-# NOINLINE t' #-}

mlift :: Functor f => Getting b a b -> Lens' (f a) (f b)
mlift l = lens (fmap (^. l)) const
{-# INLINE mlift #-}

newtype Field = F (Maybe () -> Maybe ())

field :: Field
field = F (view (mlift t'))
