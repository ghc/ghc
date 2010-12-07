{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- This is a superclass loop test
-- It should fail with a type error, but
-- it's all too easy to succeed with a bogus recursive dictionary

module SCLoop where

class SC a where 
  f :: a -> ()

class SC a => A a b where 
  op :: a -> b -> ()
  op x _ = f x

instance A a b => A a [b]
-- dfun1 :: \d::(A a b) -> DA (sc d)

instance SC a  => A a (Maybe b)
-- dfun2 :: \d::SC a -> DA d

foo = op () ([Just True])

{- Here is the explanation: 
~~~~~~~~~~~~~~~~~~~~~~~~~~~

[Wanted]  d1 : (A () [Maybe Bool])
~~~>                              d1 := dfun1 d2 
[Wanted]  d2 : (A () (Maybe Bool))
~~~>                              d2 := dfun2 d3 
[Wanted]  d3 : SC ()
[Derived] d4 : SC ()               d4 := sc d1
~~~> 
      d3 := sc d1
      isGoodRecEv will check: 
                  d3 == sc d1 
                     == sc (dfun1 d2) 
                     == sc (dfun1 (dfun2 d3) ==> PASSES!   (gravity = 1)
        This is BAD BAD BAD, because we get a loop

      If we had inlined the definitions:
                  d3 == sc d1 
                     == sc (DA (sc d2))
                     == sc (DA (sc (DA d3))) ==> DOES NOT! (gravity = 0)

We should get "No instance for SC ()"
-}








