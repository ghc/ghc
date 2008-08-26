{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}

module ShouldCompile() where

-- Trac #2497
{-# RULES "id" forall (x :: a). id x = x #-}



-- Trac #2494
foo :: (forall m. Monad m => Maybe (m a) -> Maybe (m a)) -> Maybe a -> Maybe a
foo _ x = x

{-# RULES

"foo/foo"
  forall (f :: forall m. Monad m => Maybe (m a) -> Maybe (m a))
         (g :: forall m. Monad m => Maybe (m a) -> Maybe (m a)) x.
  foo f (foo g x) = foo (f . g) x
 #-}


-- Trac #2213

eq,beq :: Eq a => a -> a -> Bool
eq = (==)    -- Used
beq = (==)   -- Unused

{-# RULES
    "rule 1" forall x y. x == y = y `eq` x
  #-}
