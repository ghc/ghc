{-# OPTIONS_GHC -XImpredicativeTypes -fno-warn-deprecated-flags #-}

module PList2 where 
-- Polymorphic lists 2: require smart-app-arg & smart-app-res: Should fail w/o smart-app-arg

type Sid = forall a. a -> a 

ids :: [Sid] 
ids = [] 

test0 :: [Sid] 
test0 = (\x -> x):ids  -- requires smart-app-res

test1 :: [Sid]	-- Added SLPJ
test1 = ids ++ test0 

test2 :: [Sid]
test2 = tail test1 -- requires smart-app-arg 

test3 :: [Sid] 	-- Added SLPJ 
test3 = reverse test2 

test4 :: Sid
test4 = head ids  --requires smart-app-arg

test5 :: Sid
test5 = head ids  -- still requires smart-app-arg 
