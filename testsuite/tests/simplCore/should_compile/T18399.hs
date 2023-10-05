module Bug where

f :: p b d -> (a -> b) -> (c -> d) -> p a c -> p b d
{-# INLINE f #-}
f = const . const . const
