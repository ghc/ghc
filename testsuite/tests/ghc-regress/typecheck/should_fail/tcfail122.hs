{-# OPTIONS -fglasgow-exts #-}
 
module ShouldFail where

-- There should be a kind error, when unifying (a b) against (c d)

foo = [ undefined :: forall a b. a b,
	undefined :: forall (c:: (* -> *) -> *) (d :: * -> *). c d ]
