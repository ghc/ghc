> data Fun = MkFun (Fun -> Fun)
> data LList a = Nill | Conss a (LList a)

> id :: Fun -> Fun
> id f = f
