{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Don't do the cunning new newtype-deriving thing
-- when the type constructor is recursive

module Main where


newtype A = A [A] deriving (Eq)

-- The derived instance would be:
-- instance Eq A where
--      (A xs) == (A ys) = xs==ys
--   $df :: Eq [A] => Eq A
--   $df d = d |> Eq (sym co)

x :: A
x = A [A [], A [A []]]

main = print (x == x)

