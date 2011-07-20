{-# LANGUAGE GADTs, KindSignatures #-}

-- See Trac #1323; crashed GHC 6.6

module Main where

data Zero = Zero
            deriving (Show)

-- Change this newtype to a data and all's well
-- and it's not like the class restriction actually restricts
-- the type rather than the constructor
newtype Succ p = Succ p deriving (Show)

class TyNum a where
instance TyNum Zero where
instance (TyNum p) => TyNum (Succ p) where

data List :: * -> * -> * where
             Nil :: List a Zero
             Cons :: (TyNum p) => a -> List a p -> List a (Succ p)

instance (Show a) => Show (List a l) where
    show Nil = "Nil"
    show (Cons a t) = "Cons " ++ (show a) ++ " (" ++ (show t) ++ ")"

zipL :: List a l -> List b l -> List (a, b) l
zipL Nil Nil = Nil -- this line is fine
zipL (Cons l ls) (Cons r rs) = Cons (l,r) (zipL ls rs) -- this line blows up

l1 = Cons 5 (Cons 3 Nil)
l2 = Cons True (Cons False Nil)

main = print $ zipL l1 l2

{-
$ ghc --make Test.hs
[1 of 1] Compiling Main             ( Test.hs, Test.o )
ghc-6.6: panic! (the 'impossible' happened)
  (GHC version 6.6 for x86_64-unknown-linux):
        Pattern match failure in do expression at simplCore/Simplify.lhs:1540:8-21

Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
-}

