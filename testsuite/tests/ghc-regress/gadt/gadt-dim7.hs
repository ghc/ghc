{-# OPTIONS_GHC -fglasgow-exts #-}

module ShouldSucceed4 where 

data Z 
data S a 


data Add n m r where 
  PZero :: Add Z m m 
  PSucc :: Add n m p -> Add (S n) m (S p) 


data XList n a where 
  XNil :: XList Z a  
  XCons :: a -> XList n a -> XList (S n) a 


-- simple safe append function 
append :: (Add n m r) -> XList n a -> XList m a -> XList r a 
append PZero XNil l = l 
append (PSucc prf) (XCons x xs) l = XCons x (append prf xs l)


