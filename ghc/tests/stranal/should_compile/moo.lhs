> module Test where
> data Moo a b = Msimple | Mcompl (Moo b a)


> idMoo :: Moo a -> Moo a
> idMoo x = x
