-- !!! Print002.hs: printing various entities in prefix/infix forms
-- !!!	(both in various syntaxes & in interfaces)

module Print002 where

-- type & data constructors

data Foo a b c
  = MkFoo1 a a
  | (:##) b c
  | b `MkFoo3` b
  | c :*** c
  deriving (Eq, Ord)

-- classes and methods

class Bar a where
    meth1, (/////) :: a -> a -> Bool
    meth2 :: a -> b -> Bool

class (Bar a) => Bar2 a	    -- no methods

-- regular values (and uses of the above)

f1  x y	    = x `MkFoo1` y
x `f1a` y    = MkFoo1 x y

x `f2` y    = (:##) x y
f2a x  y    = x :## y

(....) x y  = MkFoo3 x y
x .....  y  = x `MkFoo3` y

x <<<< y    = x :*** y
(<<<<) x y  = (:***) x y

f3a x y = meth1 x y
f3b x y = x `meth1` y
f3c x y = (/////) x y
f3d x y = x ///// y
