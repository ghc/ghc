module Test where
data Fun = MkFun (Fun -> Fun)
data LList a = Nill | Conss a (LList a)

g :: Fun -> Fun
g f = f
