{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
module Bug where
class A a oops
data D d = D d

instance A a oops => Read (D a)
-- Actually this instance is ambiguous 
-- and is now rightly rejected

data E e = E (D e) deriving Read

instance A Int Bool


