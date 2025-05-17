{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module Example1 where

-- Current behavior: These are accepted without warning
type family F a :: k
type instance F Int = Char
type instance F Int = Maybe

-- With proposed change: These would need to be written as
-- type family F a :: k
-- type instance F @Type         Int = Char
-- type instance F @(Type->Type) Int = Maybe