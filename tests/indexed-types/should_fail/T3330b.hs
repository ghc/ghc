{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Bizarrely this made 6.10 loop

module T3330b where

class RFunctor c a b where
    type Res c a b :: *
    rmap :: (a -> b) -> c -> Res c a b

instance (a ~ c) => RFunctor c a b where
    type Res c a b = b
    rmap f = f

instance (RFunctor c a b, a ~ c) => RFunctor [c] a b where
    type Res [c] a b = [b]
    rmap f = map (map f)
