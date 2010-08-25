{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances, GADTs #-}
module Main where

-- Tests that deriving works for data families
data family Foo a

data instance Foo Int
  = A | B | C | D
  deriving (Eq, Enum)

f :: Foo Int -> Bool
f A = True
f B = False
f _ = True

-- Tests that deriving works for GADTs
data Bar a where
   P :: Int -> Bar Int
   Q :: Bar Int

deriving instance (Eq (Bar Int))

main = do { print (map f [B .. D])
          ; print [P 3 == P 3, P 4 == Q] }
