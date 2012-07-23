{-# LANGUAGE TypeFamilies #-}
module Small where

class CoCCC k where
        type Coexp k :: * -> * -> *
        type Sum k :: * -> * -> *
        coapply :: k b (Sum k (Coexp k a b) a)
        cocurry :: k c (Sum k a b) -> k (Coexp k b c) a
        uncocurry :: k (Coexp k b c) a -> k c (Sum k a b)

{-# RULES
"cocurry coapply"               cocurry coapply = id
"cocurry . uncocurry"           forall x. cocurry (uncocurry x) = x
"uncocurry . cocurry"           forall x. uncocurry (cocurry x) = x
 #-}
