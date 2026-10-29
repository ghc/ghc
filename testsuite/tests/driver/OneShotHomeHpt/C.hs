{-# LANGUAGE TypeFamilies #-}
module C where

data T = MkT Int

-- A non-orphan instance: only visible once C.hi is loaded
instance Show T where
  show (MkT n) = "MkT " ++ show n

-- A non-orphan family instance
type family F a
type instance F T = Int

g :: Int -> Int
g x = x
{-# NOINLINE g #-}

{-# RULES "g/42" forall x. g x = 42 #-}
