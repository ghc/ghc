{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}

module InstEqContext2  where 

data E v a = E a
data RValue

instance (Eq a, v ~ RValue) => Eq (E v a) where
    E x == E y  =  x == y

a :: E v Int
a = undefined

foo = a == a

