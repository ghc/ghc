{-# LANGUAGE TypeFamilies #-}
module Small where

import Data.Kind (Type)

class CoCCC k where
        type Coexp k :: Type -> Type -> Type
        type Sum k :: Type -> Type -> Type
        coapply' :: k b (Sum k (Coexp k a b) a)
        cocurry' :: k c (Sum k a b) -> k (Coexp k b c) a
        uncocurry' :: k (Coexp k b c) a -> k c (Sum k a b)

coapply   :: CoCCC k => k b (Sum k (Coexp k a b) a)
{-# INLINE [1] coapply #-}
coapply = coapply'

cocurry   :: CoCCC k => k c (Sum k a b) -> k (Coexp k b c) a
{-# INLINE [1] cocurry #-}
cocurry = cocurry'

uncocurry :: CoCCC k => k (Coexp k b c) a -> k c (Sum k a b)
{-# INLINE [1] uncocurry #-}
uncocurry = uncocurry'

{-# RULES
"cocurry coapply"               cocurry coapply = id
"cocurry . uncocurry"           forall x. cocurry (uncocurry x) = x
"uncocurry . cocurry"           forall x. uncocurry (cocurry x) = x
 #-}
