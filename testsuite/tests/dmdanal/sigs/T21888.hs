{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.MemoTrie (HasTrie(..)) where

import Control.Arrow (Arrow(first))
import Data.Bits (Bits((.|.), shiftL))
import Data.Kind (Type)

infixr 0 :->:

class HasTrie a where
    data (:->:) a :: Type -> Type
    enumerate :: (a :->: b) -> [(a,b)]

instance HasTrie () where
  newtype () :->: a = UnitTrie a
  enumerate (UnitTrie a) = [((),a)]

instance HasTrie Bool where
  data Bool :->: x = BoolTrie x x
  enumerate (BoolTrie f t) = [(False,f),(True,t)]

instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where
  data (Either a b) :->: x = EitherTrie (a :->: x) (b :->: x)
  enumerate (EitherTrie s t) = enum' Left s `weave` enum' Right t

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
enum' f = (fmap.first) f . enumerate

weave :: [a] -> [a] -> [a]
[] `weave` as = as
as `weave` [] = as
(a:as) `weave` bs = a : (bs `weave` as)

instance (HasTrie a, HasTrie b) => HasTrie (a,b) where
  newtype (a,b) :->: x = PairTrie (a :->: (b :->: x))
  enumerate (PairTrie tt) =
    [ ((a,b),x) | (a,t) <- enumerate tt , (b,x) <- enumerate t ]

instance HasTrie x => HasTrie [x] where
  newtype [x] :->: a = ListTrie (Either () (x,[x]) :->: a)
  enumerate (ListTrie t) = enum' list t

list :: Either () (x,[x]) -> [x]
list = either (const []) (uncurry (:))

unbit :: Num t => Bool -> t
unbit False = 0
unbit True  = 1

unbits :: (Num t, Bits t) => [Bool] -> t
unbits [] = 0
unbits (x:xs) = unbit x .|. shiftL (unbits xs) 1

instance HasTrie Integer where
  newtype Integer :->: a = IntegerTrie ((Bool,[Bool]) :->: a)
  enumerate (IntegerTrie t) = enum' unbitsZ t

unbitsZ :: (Num n, Bits n) => (Bool,[Bool]) -> n
unbitsZ (positive,bs) = sig (unbits bs)
 where
   sig | positive  = id
       | otherwise = negate
