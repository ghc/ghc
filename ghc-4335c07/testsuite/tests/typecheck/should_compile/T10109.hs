{-# LANGUAGE PolyKinds, MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module T10109 where

data Succ a

class Add (a :: k1) (b :: k2) (ab :: k3) | a b -> ab
instance (Add a b ab) => Add (Succ a) b (Succ ab)

