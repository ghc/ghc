{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module T12522 where

import Data.Kind (Type)

foo = f (Just 'c')

data D1 (x :: Type)
data D2

type family TF x = t | t -> x
type instance TF (D1 x, a) = Maybe (TF (x, a))
type instance TF (D2, ()) = Char

f :: TF (x, a) -> ()
f _ = ()

foo1 = f_good (Just 'c')
foo2 = f_bad (Just 'c')

type family TF2 x y = t | t -> x y
type instance TF2 Int Float = Char

type family TF_Good x y = t | t -> x y
type instance TF_Good a (Maybe x) = Maybe (TF2 a x)

f_good :: TF_Good a x -> ()
f_good _ = ()

type family TF_Bad x y = t | t -> x y
type instance TF_Bad (Maybe x) a = Maybe (TF2 a x)

f_bad :: TF_Bad x a -> ()
f_bad _ = ()

{-

Maybe Char ~ TF (xx, aa)


Model [D] s_aF4 ~ Maybe Char

       [W] TF (x_aDY, a_aJn) ~ s_aF4    FunEq
--> {aJn = aJp)
       [W} TF (x_aDY, a_aJp) ~ s_aF4    FunEq
--> {new derived equalities}
       [D] x_aDY ~ D1 x_aJq
       [D] a_aJp ~ a_aJR
-}
