{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}

module M where
import Data.Kind (Type)

type F :: Type -> Type
type family F

type Prod :: Type -> Type -> Type
type family Prod (a :: Type) (b :: Type) :: Type

und :: F Int
und = und

f :: a -> Prod (F Int) a -> Prod a a
f = f

repMap :: Prod (F Int) (F Int) -> Prod (F Int) (F Int)
repMap = f und


{- This is what went wrong in GHC 9.8

Inert: [W] Prod (F Int) a ~ Prod a a
Work: [W] Prod (F Int) (F Int) ~ Prof (F Int) a

---> rewrite with inert
  [W] Prod (F Int) (F Int) ~ Prod a a
---> swap (meta-var to left)
  [W] Prod a a ~ Prod (F Int) (F Int)

Kick out the inert

Inert: [W] Prod a a ~ Prod (F Int) (F Int)
Work: [W] Prod (F Int) a ~ Prod a a

--> rewrite with inert
    [W] Prod (F Int) a ~ Prod (F Int) (F Int)
--> swap (size)
    [W] Prod (F Int) (F Int) ~ Prod (F Int) a

Kick out the inert

Inert: [W] Prod (F Int) (F Int) ~ Prod (F Int) a
Work: [W] Prod a a ~ Prod (F Int) (F Int)

--> rewrite with inert
    [W] Prod a a ~ Prod (F Int) a
--> swap (size)
    [W] Prof (F Int) a ~ Prod a a


-}
