{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -ddump-simpl-stats #-}

-- The rule foo/bar should fire

module Roman where

foo :: (forall m. m a -> m b) -> m a -> m b
{-# NOINLINE foo #-}
foo f = f

bar :: (forall m. m a -> m a) -> m a -> m a
bar f = f

{-# RULES "foo/bar" foo = bar #-}

blip = foo id

