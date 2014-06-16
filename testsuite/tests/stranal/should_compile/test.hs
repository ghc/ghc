module Test where
data LList t = Nill | Conss t (LList t)
data BBool = TTrue | FFalse

f Nill = TTrue
f (Conss a as) = FFalse
