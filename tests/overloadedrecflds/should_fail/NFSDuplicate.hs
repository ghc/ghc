{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoDuplicateRecordFields #-}
module NFSDuplicate where

-- Two definitions of 'foo' as fields is an error, even though it is permitted
-- to define it as a non-field.
data S = MkS { foo :: Int }
data T = MkT { foo :: Int }

foo = ()
