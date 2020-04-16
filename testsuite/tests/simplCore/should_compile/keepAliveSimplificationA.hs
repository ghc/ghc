{-# LANGUAGE MagicHash #-}

module Hi (g) where

import GHC.Prim
import GHC.IO
import GHC.Int

keepAlive :: a -> IO r -> IO r
keepAlive x f = IO $ \s -> keepAlive# x (unIO f) s
{-# INLINE keepAlive #-}

f :: a -> IO Int
f x = keepAlive x $ return 41

-- The 'succ' should be folded into the continuation given to
-- keepAlive; constant folding will then turn the 41# into a 42#, which is what
-- we check for in this test.
g :: a -> IO Int
g x = succ <$> f x
