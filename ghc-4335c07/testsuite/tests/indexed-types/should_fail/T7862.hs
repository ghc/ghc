{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

-- This used to fail because of the silent-superclass
-- mechanism, but now it succeeds as it should

module T7862 where

type family Scalar t

newtype Tower s a = Tower [a]

type instance Scalar (Tower s a) = a

class (Num (Scalar t), Num t) => Mode t where
    (<+>) :: t -> t -> t

instance Num a => Mode (Tower s a) where
    Tower as <+> _ = undefined
      where
        _ = (Tower as) <+> (Tower as)

instance Num a => Num (Tower s a) where
