module Serial (unfoldrM) where

import StreamK (IsStream)
import qualified StreamD as D

{-# INLINE unfoldrM #-}
unfoldrM :: (IsStream t, Monad m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM step seed = D.fromStreamD (D.unfoldrM step seed)
