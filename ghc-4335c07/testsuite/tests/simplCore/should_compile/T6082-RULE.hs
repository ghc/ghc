module T6082_RULE where

-- Should warn
foo1 x = x
{-# RULES "foo1" forall x. foo1 x = x #-}

-- Should warn
foo2 x = x
{-# INLINE foo2 #-}
{-# RULES "foo2" forall x. foo2 x = x #-}

-- Should not warn
foo3 x = x
{-# NOINLINE foo3 #-}
{-# RULES "foo3" forall x. foo3 x = x #-}

