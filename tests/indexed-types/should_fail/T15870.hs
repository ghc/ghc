{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module T15870 where

data Optic a where
  --Index :: Nat    -> Optic a
  --Name  :: Symbol -> Optic a
  (:.:) :: Optic a -> Optic b -> Optic a -- composition

class Gettable a (optic :: Optic a) where
  type Get a (optic :: Optic a)

{-
some basic instances, e.g.
instance Gettable (a,b) (Index 0) where
  type Get (a,b) (Index 0) = a
...
-}

instance forall a b (g1 :: Optic a) (g2 :: Optic b).
        ( Gettable a g1
        , b ~ Get a g1
        , Gettable b g2
        ) => Gettable a (g1 :.: g2) where
  type Get a (g1 :.: g2) = Get a g2
