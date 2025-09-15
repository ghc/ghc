module BadErr where

data T f a = T (f a)

class C f where
  method :: f a -> r

-- badErr :: C c1 => T c1 b -> r
-- badErr = method

worseErr :: (C c1, C c2) => T c1 b -> T c2 b -> r
worseErr = method

foo :: T c a -> r
foo = undefined

bar1, bar2 :: C f => f a -> r
bar1 = foo
bar2 = foo
