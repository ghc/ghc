{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module T5300 where

import Control.Monad.Trans.State (StateT)

class C1 a b c | a -> b
class C2 a b c

data T b = T

f1 :: (Monad m, C1 a b c) => a -> StateT (T b) m a
f1 f = undefined

f2 :: (Monad m, C1 a1 b1 c1, C2 a2 b2 c2) => a1 -> StateT (T b2) m a2
f2 fm = f1 fm >>= return . undefined
