{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE PolyKinds                  #-}

module T5717 where


data TypeRep = TypeRep

data Proxy t = Proxy

typeRep :: Proxy a -> TypeRep
typeRep Proxy = TypeRep

-- This one works fine:
typeOf :: forall a. a -> TypeRep
typeOf _ = typeRep (Proxy :: Proxy a)

-- But this one panics!
typeOf1 :: forall t a. t a -> TypeRep
typeOf1 _ = typeRep (Proxy :: Proxy t)
