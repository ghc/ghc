{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module T4175 where

import GHC.Exts

type family A a b
type instance A Int Int = ()
type instance A (Maybe a) a = a
type instance A (B a) b = ()

data family B a
data instance B () = MkB

class C a where
    type D a b

instance C Int where
    type D Int () = String

instance C () where
    type D () () = Bool

type family E a where
    E ()  = Bool
    E Int = String

class Z a

class F (a :: Constraint)
instance F (Z a)

class G (a :: * -> *)
instance G B
