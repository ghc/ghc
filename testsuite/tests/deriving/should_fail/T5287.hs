{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
module Bug where
class A a oops
data D d = D d
instance A a oops => Read (D a)
data E e = E (D e) deriving Read


