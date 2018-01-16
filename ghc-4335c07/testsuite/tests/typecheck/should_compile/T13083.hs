{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}

-- | Bug(?) in Coercible constraint solving

module T13083 where

import GHC.Generics (Par1(..),(:*:)(..))
import GHC.Exts (coerce)

-- Representation as free vector space
type family V (a :: *) :: * -> *

type instance V R = Par1
type instance V (a,b) = V a :*: V b

type instance V (Par1 a) = V a

data R = R

-- Linear map in row-major order
newtype L a b = L (V b (V a R))

-- Use coerce to drop newtype wrapper
bar :: L a b -> V b (V a R)
bar = coerce

{-
[W] L a b ~R V b (V a R)
-->
    V b (V a R)  ~R  V b (V a R)
-}

{--------------------------------------------------------------------
    Bug demo
--------------------------------------------------------------------}

-- A rejected type specialization of bar with a ~ (R,R), b ~ (Par1 R,R)
foo :: L (R,R) (Par1 R,R) -> V (Par1 R,R) (V (R,R) R)
-- foo :: L (a1,R) (Par1 b1,b2) -> V (Par1 b1,b2) (V (a1,R) R)
foo = coerce

{-
[W] L (a1,R) (Par1 b1, b2)   ~R   V (Par1 b1,b2) (V (a1,R) R)
-->
    V (Par1 b1, b2) (V (a1,R) R)  ~R    same

    -> (V (Par1 b1) :*: V b2) ((V a1 :*: V R) R)
    -> (:*:) (V b1) (V b2) (:*: (V a1) Par1 R)

-->
    L (a1,R) (Par1 b1, b2)   ~R   (:*:) (V b1) (V b2) (:*: (V a1) Par1 R)
-}

--     • Couldn't match representation of type ‘V Par1’
--                                with that of ‘Par1’
--         arising from a use of ‘coerce’

-- Note that Par1 has the wrong kind (* -> *) for V Par1

-- Same error:
--
-- foo :: (a ~ (R,R), b ~ (Par1 R,R)) => L a b -> V b (V a R)

-- The following similar signatures work:

-- foo :: L (R,R) (R,Par1 R) -> V (R,Par1 R) (V (R,R) R)
-- foo :: L (Par1 R,R) (R,R) -> V (R,R) (V (Par1 R,R) R)

-- Same error:

-- -- Linear map in column-major order
-- newtype L a b = L (V a (V b s))

-- foo :: L (R,R) (Par1 R,R) -> V (R,R) (V (Par1 R,R) R)
-- foo = coerce

