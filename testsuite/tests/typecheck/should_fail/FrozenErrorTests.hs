{-# LANGUAGE RankNTypes, GADTs, TypeFamilies #-}
module Test where 


data T a where 
  MkT :: a -> T a 
  MkT3 :: forall a. (a ~ Bool) => T a 

-- Mismatches in givens 
bloh :: T Int -> () 
bloh x = case x of 
           MkT3 -> () 

type family F a b 
type family G a b
type instance F a Bool = a 
type instance G a Char = a

goo1 :: forall a b. (F a b ~ [a]) => b -> a  -> a
goo1 = undefined 

goo2 :: forall a. G a Char ~ [Int] => a -> a
goo2 = undefined

-- Just an occurs check
test1 = goo1 False undefined

-- A frozen occurs check, now transformed to decomposition error
test2 = goo2 (goo1 False undefined)
test3 = goo1 False (goo2 undefined)


-- A frozen occurs check, now transformed to both a decomposition and occurs check
data M a where 
  M :: M a 
data T2 a b where
  T2 :: T2 a b 

goo3 :: forall a b. F a b ~ T2 (M a) a => b -> a -> a 
goo3 = undefined

goo4 :: forall a c. G a Char ~ T2 (T2 c c) c => a -> a
goo4 = undefined 

test4 = goo4 (goo3 False undefined)
test5 = goo3 False (goo4 undefined)



