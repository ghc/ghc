{-# LANGUAGE BlockArguments, GADTs, LambdaCase #-}

module T18013a where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow
import Data.Functor

data Result s a = Result !s a

data Rule m a b where
  Rule :: !s -> !(s -> a -> m (Result s b)) -> Rule m a b

mkRule :: Functor m => s -> (s -> a -> m (Result s b)) -> Rule m a b
mkRule = Rule
{-# INLINE CONLIKE [1] mkRule #-}
{-# RULES
"mkRule @((), _)" forall s f. mkRule ((), s) f =
  Rule s (\s1 a -> f ((), s1) a <&> \(Result ((), s2) b) -> Result s2 b)
"mkRule @(_, ())" forall s f. mkRule (s, ()) f =
  Rule s (\s1 a -> f (s1, ()) a <&> \(Result (s2, ()) b) -> Result s2 b)
#-}

instance Monad m => Category (Rule m) where
  id = arr id
  {-# INLINE id #-}
  Rule t0 g . Rule s0 f = mkRule (s0, t0) \(s1, t1) a -> do
    Result s2 b <- f s1 a
    Result t2 c <- g t1 b
    pure $! Result (s2, t2) c
  {-# INLINE (.) #-}

instance Monad m => Arrow (Rule m) where
  arr f = Rule () \_ a -> pure $! Result () (f a)
  {-# INLINE arr #-}
  first (Rule s0 f) = Rule s0 \s1 (a, c) -> do
    Result s2 b <- f s1 a
    pure $! Result s2 (b, c)
  {-# INLINE first #-}

instance Monad m => ArrowChoice (Rule m) where
  left (Rule s0 f) = Rule s0 \s1 -> \case
    Left a -> do
      Result s2 b <- f s1 a
      pure $! Result s2 (Left b)
    Right a ->
      pure $! Result s0 (Right a)
  {-# INLINE left #-}
  Rule s0 f ||| Rule t0 g = mkRule (s0, t0) \(s1, t1) -> \case
    Left a -> do
      Result s2 b <- f s1 a
      pure $! Result (s2, t0) b
    Right a -> do
      Result t2 b <- g t1 a
      pure $! Result (s0, t2) b
  {-# INLINE (|||) #-}
