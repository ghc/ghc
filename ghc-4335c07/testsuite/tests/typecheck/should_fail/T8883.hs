{-# LANGUAGE TypeFamilies #-}

-- Trac #8883

module T8883 where

type family PF a :: * -> *

class Regular a where
  from :: a -> PF a a

-- For fold we infer following type signature:
--
-- fold :: (Functor (PF a), Regular a) => (PF a b -> b) -> a -> b
--
-- However, this signature requires FlexibleContexts since the first
-- type-class constraint is not of the form (class type-variable) nor
-- (class (type-variable type1 type2 ... typen)). Since this extension
-- is not enabled compilation should fail.
fold f = f . fmap (fold f) . from
