{-# OPTIONS_GHC -fglasgow-exts #-}

module PList1 where 
-- Polymorphic lists 1: requires smart-app-res  

type Sid = forall a . a -> a

ids :: [Sid] 
ids = [] 

-- requires smart-app-res 
test0 :: [Sid] 
test0 = (\x->x) : ids 

test1 :: [Sid]	-- SLPJ added
test1 = ids ++ test0 

test2 :: [Sid] 
test2 = tail test1 


test3 :: [Sid]	-- SLPJ added
test3 = reverse test2 
test4 = (tail::([Sid]->[Sid])) test2 

test5 = (head::([Sid]->Sid)) test2 