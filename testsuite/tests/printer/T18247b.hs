{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module T18247b where

import Control.Monad (guard)
import qualified Data.Sequence as Seq

pattern P = 42

--useP P = 43

--

data Type = App String [Type]

pattern Arrow :: Type -> Type -> Type
pattern Arrow t1 t2 = App "->" [t1, t2]

pattern Int = App "Int" []

pattern Maybe t = App "Maybe" [t]


{-
collectArgs :: Type -> [Type]
collectArgs (Arrow t1 t2) = t1 : collectArgs t2
collectArgs _ = []

isInt :: Type -> Bool
isInt Int = True
isInt _ = False

isIntEndo :: Type -> Bool
isIntEndo (Arrow Int Int) = True
isIntEndo _ = False

arrows :: [Type] -> Type -> Type
arrows = flip $ foldr Arrow
-}

--


pattern Empty <- (Seq.viewl -> Seq.EmptyL)
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs)
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x)

{-
viewPL (x :< Empty) = x
viewPR (Empty :> y) = y
-}

--


pattern Succ n <-
  (\x -> (x -1) <$ guard (x > 0) -> Just n)
  where
    Succ n = n + 1

{-
fac (Succ n) = Succ n * fac n
fac 0 = 1
-}

--


data Showable where
  MkShowable :: (Show a) => a -> Showable

-- Required context is empty, but provided context is not
pattern Sh :: () => (Show a) => a -> Showable
pattern Sh x <- MkShowable x

{-
showable :: (Show a) => a -> Showable
showable x = MkShowable x
-}

--


-- Provided context is empty
pattern One :: (Num a, Eq a) => a
pattern One <- 1


-- one One = 2

--


pattern Pair x y <- [x, y]



{-
f (Pair True True) = True
f _ = False

g [True, True] = True
g _ = False
-}



--

data Nat = Z | S Nat deriving (Show)

pattern Ess p = S p


--two = S ( S Z)

--

pattern Single x = [x]

pattern Head x <- x : xs

{- single (Single x) = x
hd :: [a] -> a
hd (Head x) = x
-}

--


data T a where
  MkT :: (Show b) => a -> b -> T a

pattern ExNumPat x = MkT 42 x

{-
h :: (Num t, Eq t) => T t -> String
h (ExNumPat x) = show x
-}
