-- We use fromStreamK/toStreamK to convert the direct style stream to CPS
-- style. In the first phase we try fusing the fromStreamK/toStreamK using:
--
-- {-# RULES "fromStreamK/toStreamK fusion"
--     forall s. toStreamK (fromStreamK s) = s #-}
--
-- If for some reason some of the operations could not be fused then we have
-- fallback rules in the second phase. For example:
--
-- {-# INLINE_EARLY unfoldr #-}
-- unfoldr :: (Monad m, IsStream t) => (b -> Maybe (a, b)) -> b -> t m a
-- unfoldr step seed = fromStreamS (S.unfoldr step seed)
-- {-# RULES "unfoldr fallback to StreamK" [1]
--     forall a b. S.toStreamK (S.unfoldr a b) = K.unfoldr a b #-}```
--
-- Then, fromStreamK/toStreamK are inlined in the last phase:
--
-- {-# INLINE_LATE toStreamK #-}
-- toStreamK :: Monad m => Stream m a -> K.Stream m a```
--
-- The fallback rules make sure that if we could not fuse the direct style
-- operations then better use the CPS style operation, because unfused direct
-- style would have worse performance than the CPS style ops.

#define INLINE_EARLY  INLINE [2]
#define INLINE_NORMAL INLINE [1]
#define INLINE_LATE   INLINE [0]
