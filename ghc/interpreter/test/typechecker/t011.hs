--!!! Escaping existential variable II

data Appl = MkAppl (a -> Int) a (a -> a)

bad3 y              = let g (MkAppl f x i) = length [x,y] + 1
                      in  True
