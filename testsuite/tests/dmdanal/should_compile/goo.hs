module Test where
data Goo a = Gsimpl | Gcompl ([Goo a]) 
data Moo a b = Msimple | Mcompl (Moo b a)


idGoo :: Goo a -> Goo a
idGoo x = x

idMoo :: Moo a b -> Moo a b
idMoo x = x
