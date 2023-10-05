-- Regression test for #495

-- inlining an undeclared identifier should give error, not panic...
{-# INLINE blarg #-}

-- even if the identifier is imported in the Prelude...
{-# INLINE lookup #-}
