{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module TypeDirectedComplete where

data Proxy a = Proxy

class IsEmpty a where
  isEmpty :: a -> Bool

class IsCons a where
  type Elt a
  isCons :: a -> Maybe (Elt a, a)

pattern Empty :: IsEmpty a => a
pattern Empty <- (isEmpty -> True)

pattern Cons :: IsCons a => Elt a -> a -> a
pattern Cons x xs <- (isCons -> Just (x,xs))

instance IsEmpty (Proxy a) where
  isEmpty Proxy = True

instance IsEmpty [a] where
  isEmpty = null

instance IsCons [a] where
  type Elt [a] = a
  isCons [] = Nothing
  isCons (x:xs) = Just (x,xs)

{-# COMPLETE Empty :: Proxy a #-}
{-# COMPLETE Empty, Cons :: [a] #-}

foo :: Proxy a -> Int
foo Empty = 0

bar :: [a] -> Int
bar Empty = 0
bar (Cons _ _) = 1

baz :: [a] -> Int
baz Empty = 0
