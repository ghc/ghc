--!!! Escaping existential variable I

data Appl = MkAppl (a -> Int) a (a -> a)

bad1 (MkAppl f x i) = x
