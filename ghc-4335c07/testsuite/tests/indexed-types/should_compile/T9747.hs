{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, TypeFamilies, TypeOperators #-}

module T9747 where
import Data.List (intercalate)
import Data.Proxy
import GHC.Exts (Constraint)

data HList :: [*] -> * where
  Nil :: HList '[]
  Cons :: a -> HList as -> HList (a ': as)

type family HListAll (c :: * -> Constraint) (ts :: [*]) :: Constraint where
  HListAll c '[] = ()
  HListAll c (t ': ts) = (c t, HListAll c ts)

showHList :: HListAll Show ts => HList ts -> String
showHList = ("[" ++ ) . (++"]") . intercalate ", " . go
  where
    go :: HListAll Show ts => HList ts -> [String]
    go Nil = []
    go (Cons x xs) = show x : go xs

-- Things work okay up to this point
test :: String
test = showHList (Cons (2::Int)
                 (Cons (3.1 :: Float)
                 (Cons 'c' Nil)))

type family ConFun (t :: *) :: * -> Constraint
data Tag
type instance ConFun Tag = Group

class (Show a, Eq a, Ord a) => Group a

-- This is notionally similar to showHList
bar :: HListAll (ConFun l) ts => Proxy l -> HList ts -> ()
bar _ _ = ()

baz :: (ConFun l a, ConFun l b) => Proxy l -> HList [a,b] -> ()
baz = bar
