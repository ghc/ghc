{-# LANGUAGE GADTs #-}

-- Trac #2520: a bug in the specialiser when we tried to
-- quantify over an Internal Name

module Types where

data Prod a b = Prod a b

data Nil = Nil

class ProdSel f where
   nil :: f Nil
   prod :: f x -> f y -> f (Prod x y)

instance ProdSel SqlFields where
   nil = SFNil
   prod = SFProd

{-# SPECIALIZE reproject :: SqlFields a -> SqlFields a #-}

reproject :: ProdSel f => SqlFields a -> f a
reproject SFNil = nil
reproject (SFProd a b) = prod (reproject a) (reproject b)

data SqlFields a where
  SFNil :: SqlFields Nil
  SFProd :: SqlFields a -> SqlFields b -> SqlFields (Prod a b)
