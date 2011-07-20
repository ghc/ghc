{-# LANGUAGE ExistentialQuantification #-}

-- Exmaples from Doaitse Swierestra and Brandon Moore
-- GHC users mailing list, April 07, title "Release plans"

-- This one should fail, but Hugs passes it

module ShouldFail where

data T s = forall x. T (s -> (x -> s) -> (x, s, Int))

run :: T s -> Int
run ts  = case ts of
             T g -> let (x,_, b) =  g x id
                    in b

