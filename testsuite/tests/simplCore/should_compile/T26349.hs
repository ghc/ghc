{-# LANGUAGE DeepSubsumption, RankNTypes #-}
module T26349 where

{-# SPECIALIZE INLINE mapTCMT :: (forall b. IO b -> IO b) -> IO a -> IO a #-}
mapTCMT :: (forall b. m b -> n b) -> m a -> n a
mapTCMT f m = f m

{-
 We'll check
    tcExpr (mapTCMT) (Check ((forall b. IO b -> IO b) -> IO a_sk -> IO a_sk))
-}
