 {-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}  
module T4272 where

class Family f where
   terms :: f a -> a

class Family (TermFamily a) => TermLike a where
   type TermFamily a :: * -> *

laws :: forall a b. TermLike a => TermFamily a a -> b  
laws t = prune t (terms (undefined :: TermFamily a a))

prune :: TermLike a => TermFamily a a -> TermFamily a a -> b
prune = undefined  

-- terms :: Family f => f a -> a
--    Instantiate with f = TermFamily a
-- terms :: Family (TermFamily a) => TermFamily a a -> a
-- (terms (undefined::TermFamily a a) :: Family (TermFamily a) => a
-- So the call to prune forces the equality
--    TermFamily a a ~ a
-- which triggers an occurs check