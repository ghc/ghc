{-# LANGUAGE ImplicitParams, TypeFamilies #-}

module Bug where

type family St (a :: k) :: *
type family Ev (a :: k) :: * -> *

data T1 a = C1 a
data T2 h g = C2 (h ())

class (h ~ Ev g, s ~ St g) => Ren s h g

f1 ::
  (s ~ St h, Ren s h g, ?settings :: settings)
  => (g v -> g ()) -> g v -> g ()
f1 form = (\_ a -> a) (C1 (f2 {-@g-})) form

f2 :: forall g h s. ( s ~ St h, Ren s h g) => T2 h g
f2 = error "urk"

{- Call of f2

[W] s ~ St h  -->  St g ~ St h -->  St g ~ St (Ev g)
[W] Ren s h g
[W] s ~ St g   -- Superclass of wanted
[W] h ~ Ev g   -- Superclass of wanted
-}

{-
f3 ::
  T1 ()
  -> (g v -> g ())
  -> g v -> g ()
f3 wd form = ((\_ a -> a) wd form)

-}
