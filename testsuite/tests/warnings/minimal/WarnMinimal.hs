module WarnMinimal where

class Fine a where
instance Fine Int

-------------------

class Foo a where
  foo1 :: a
  foo2 :: a
  foo1 = foo2
  foo2 = foo1
  {-# MINIMAL foo1 | foo2 #-}

-- this should generate a warning
instance Foo Int where  -- WARNING LINE

-- this should generate no warning
instance Foo Char where
  foo1 = 'x'

-- nor should this
instance Foo Bool where
  foo2 = True

instance Foo Double where
  foo1 = 1
  foo2 = 2

-------------------

class Monad' f where
  return' :: a -> f a
  fmap' :: (a -> b) -> f a -> f b
  join' :: f (f a) -> f a
  bind' :: f a -> (a -> f b) -> f b
  {-# MINIMAL return', (fmap',join' | bind') #-}
  fmap' f x = bind' x (return' . f)
  join' x = bind' x id
  bind' x f = join' (fmap' f x)

instance Monad' [] where
  return' = return
  fmap' = map
  join' = concat
  -- no warning

instance Monad' Maybe where
  return' = Just
  bind' = (>>=)
  -- no warning

instance Monad' IO where
  return' = return
  bind' = (>>=)
  fmap' = fmap
  join' = (>>= id)
  -- no warning

instance Monad' ((->) e) where  -- WARNING LINE
  return' = const
  fmap' = (.)
  -- warning!

newtype Id a = Id a
instance Monad' Id where  -- WARNING LINE
  fmap' f (Id x) = Id (f x)
  join' (Id x) = x
  -- warning!

newtype Id2 a = Id2 a
instance Monad' Id2 where  -- WARNING LINE
  fmap' f (Id2 x) = Id2 (f x)
  join' (Id2 x) = x
  bind' (Id2 x) f = f x
  -- warning!

newtype Id3 a = Id3 a
instance Monad' Id3 where  -- WARNING LINE

---------

-- incorrect minimal spec
class Cheater a where  -- WARNING LINE
  cheater :: a
  {-# MINIMAL #-} -- warning!

class Cheater2 a where
  _cheater2 :: a
  {-# MINIMAL #-} -- no warning

class Cheater3 a where  -- WARNING LINE
  cheater3, cheater3b :: a
  {-# MINIMAL cheater3 #-} -- warning!

---------

-- new style warning for classes without explicit spec
instance Num Bool where  -- WARNING LINE

class NoExplicit a where
  needed :: a
  _optional :: a

instance NoExplicit Int where  -- WARNING LINE


---------
data Blarg = Blarg
class Eq' a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool
  x === y = not (x /== y)
  x /== y = not (x === y)
  {-# MINIMAL (===) | (/==) #-}
instance Eq' Blarg where  -- WARNING LINE
