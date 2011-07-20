{-# LANGUAGE GADTs #-}

module T3950 where

-- Id :: (* -> * -> *) -> * -> * -> *
data Id p x y = Id (p x y)

-- Sealed :: (* -> *) -> *
data Sealed p where
    Sealed :: p x -> Sealed p

-- w :: (* -> * -> *) -> *
-- Id p :: * -> * -> *
rp :: Bool -> Maybe (w (Id p))
rp _ = Just rp'
     where rp' :: Sealed (Id p x)
           rp' = undefined
