{-# LANGUAGE RankNTypes #-}

module Bug where

data T a = MkT

out :: forall a. T a -> ()
out MkT = ()

inHoisted :: forall r. () -> (forall a. T a -> r) -> r
inHoisted _ foo = foo MkT

inUnhoisted :: () -> forall r. (forall a. T a -> r) -> r
inUnhoisted _ foo = foo MkT

testHoisted :: ()
testHoisted = inHoisted () out

testUnhoisted :: ()
testUnhoisted = inUnhoisted () out


----------------

data A s = A { unA :: () }

runA1 :: (forall s. A s) -> ()
runA1 a = unA a

-- doesn't work :(
runA2 :: (forall s. A s) -> ()
runA2 (A a) = a

runA3 :: (forall s. A s) -> ()
runA3 a = case a of A x -> x

runA4 :: (forall s. A s) -> ()
runA4 a = let A x = a in x

runA5 :: (forall s. A s) -> ()
runA5 a = go a
  where go (A a) = a
