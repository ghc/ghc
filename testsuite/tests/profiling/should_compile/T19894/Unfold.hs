{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification  #-}
module Unfold
    ( Unfold (..)
    , supplyFirst
    , many
    , lmap
    )

where

import Step (Step(..))
#if defined(FUSION_PLUGIN)
import Fusion.Plugin.Types (Fuse(..))
#endif

data Unfold m a b =
    -- | @Unfold step inject@
    forall s. Unfold (s -> m (Step s b)) (a -> m s)

{-# INLINE [1] lmap #-}
lmap :: (a -> c) -> Unfold m c b -> Unfold m a b
lmap f (Unfold ustep uinject) = Unfold ustep (uinject Prelude.. f)

{-# INLINE [1] supplyFirst #-}
supplyFirst :: a -> Unfold m (a, b) c -> Unfold m b c
supplyFirst a = lmap (a, )

#if defined(FUSION_PLUGIN)
{-# ANN type ConcatState Fuse #-}
#endif
data ConcatState s1 s2 = ConcatOuter s1 | ConcatInner s1 s2

-- | Apply the second unfold to each output element of the first unfold and
-- flatten the output in a single stream.
--
-- /Since: 0.8.0/
--
{-# INLINE [1] many #-}
many :: Monad m => Unfold m a b -> Unfold m b c -> Unfold m a c
many (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return $ ConcatOuter s

    {-# INLINE [0] step #-}
    step (ConcatOuter st) = do
        r <- step1 st
        case r of
            Yield x s -> do
                innerSt <- inject2 x
                return $ Skip (ConcatInner s innerSt)
            Skip s    -> return $ Skip (ConcatOuter s)
            Stop      -> return Stop

    step (ConcatInner ost ist) = do
        r <- step2 ist
        return $ case r of
            Yield x s -> Yield x (ConcatInner ost s)
            Skip s    -> Skip (ConcatInner ost s)
            Stop      -> Skip (ConcatOuter ost)
