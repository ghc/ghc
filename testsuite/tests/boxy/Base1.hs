{-# OPTIONS_GHC -XImpredicativeTypes -fno-warn-deprecated-flags #-}

module Base1 where 
-- basic examples of impredicative instantiation of variables 

data MEither a b = MLeft a 
                 | MRight b 
                 | MEmpty

type Sid = forall a. a -> a 

-- no need for impredicativity 
test0 = MRight id 

-- requires impredicativity 
test1 :: Sid -> MEither Sid b
test1 fid = MLeft fid 

test2 :: MEither b Sid -> Maybe (Sid,Sid) 
test2 m = case (test1 id) of 
             MLeft x -> case m of 
                          MRight y -> Just (x,y) 
                          _ -> Nothing 
             _ -> Nothing

test3 :: MEither a b -> b
test3 (MRight x) = x 

test4 = test3 (test1 id) 

