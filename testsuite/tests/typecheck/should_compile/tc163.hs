{-# LANGUAGE RankNTypes #-}

-- This one killed GHC 5.05 and earlier
-- The problem was in a newtype with a record selector, with
-- a polymorphic argument type.  MkId generated a bogus selector
-- function

module ShouldCompile where

type M3 a = forall r. (forall b. M3' b -> (b -> M3' a) -> r) -> r

newtype M3' a = M3' { mkM3' :: M3 a }

flop :: forall a b. M3' b -> (b -> M3' a) -> Int
flop = \m' k -> mkM3' m' (\bm k1 -> error "urk")

-- Suppose mkM3' has the straightforward type: 
--     mkM3' :: forall a. M3' a -> M3 a
-- Then (mkM3' m') :: forall r. (forall b. ...) -> r
-- If we simply do a subsumption check of this against
--    alpha -> Int
-- where alpha is the type inferred for (\bm k1 ...) 
-- this won't work.

-- But if we give mkM3' the type 
--     forall a r. M3' a -> (forall b. ...) -> r
-- everthing works fine.  Very very delicate.

---------------- A more complex case -------------
bind :: M3 a -> (a -> M3 b) -> M3 b
bind m k b = b (M3' m) (\a -> M3' (k a))

observe :: M3 a -> a
observe m
      = m (\m' k -> mkM3' m'
                (\bm k1 -> observe (bind (mkM3' bm)
                        (\a -> bind (mkM3' (k1 a)) (\a -> mkM3' (k a)))))
                )

