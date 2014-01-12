{-# LANGUAGE TypeFamilies, PolyKinds, MultiParamTypeClasses #-}
module T7278 where
 
type family TF (t  :: k) :: * -> * -> *

class C (t :: k) (dcs :: * -> * -> *)

f :: (C (t :: k) (TF t)) => TF t p1 p0 -> t p1 p0
f = undefined -- panic caused by (t :: k) in the signature's context
