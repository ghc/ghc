{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeInType #-} -- or PolyKinds
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module T15352 where

import Data.Kind

class C (x :: Type) (y :: k) where
  type F y


type Hom k = k -> k -> Type

type family Ob (p :: Hom k) :: k -> Constraint

class ( obP ~ Ob p
      , opP ~ Dom p
      , obQ ~ Ob q
      , opQ ~ Dom q
      , p ~ Dom f
      , q ~ Cod f
      ) => Functor' (obP :: i -> Constraint)
                    (opP :: Hom i)
                    (p :: Hom i)
                    (obQ :: j -> Constraint)
                    (opQ :: Hom j)
                    (q :: Hom j)
                    (f :: i -> j) where
  type Dom f :: Hom i
  type Cod f :: Hom j
