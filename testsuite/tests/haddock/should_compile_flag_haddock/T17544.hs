{-# LANGUAGE TypeFamilies, GADTSyntax #-}

module T17544 where

class C1 a where
  f1 :: a -> Int
    -- ^ comment on Int

class C2 a where
  f2 :: a -> Int
  -- ^ comment on f2

class C3 a where
  f3 :: a -> Int
-- ^ comment on C3

class C4 a where
  f4 :: a -> Int
-- ^ comment
  g4 :: a -> Int

class C5 a where { data D5 a }
instance C5 Int where
  data D5 Int where
    MkD5 :: D5 Int
     -- ^ comment on D5 Int

class C6 a where { data D6 a }
instance C6 Int where
  data D6 Int where
    MkD6 :: D6 Int
    -- ^ comment on MkD6

class C7 a where { data D7 a }
instance C7 Int where
  data D7 Int where
    MkD7 :: D7 Int
   -- ^ comment on data instance D7 Int

class C8 a where { data D8 a }
instance C8 Int where
  data D8 Int where
    MkD8 :: D8 Int
  -- ^ comment on data instance D8 Int

class C9 a where { data D9 a }
instance C9 Int where
  data D9 Int where
    MkD9 :: D9 Int
 -- ^ comment on class instance C9 Int

class C10 a where { data D10 a }
instance C10 Int where
  data D10 Int where
    MkD10 :: D10 Int
-- ^ comment on class instance C10 Int
