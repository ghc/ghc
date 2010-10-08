
{-# LANGUAGE Generics, TypeOperators #-}

-- !!! Test generics
module Main where

import GHC.Base

class Bin a where
  toBin   :: a -> [Int]
  fromBin :: [Int] -> (a, [Int])

  toBin {| Unit |}    Unit	= []
  toBin {| a :+: b |} (Inl x)   = 0 : toBin x
  toBin {| a :+: b |} (Inr y)   = 1 : toBin y
  toBin {| a :*: b |} (x :*: y) = toBin x ++ toBin y


  fromBin {| Unit |}    bs      = (Unit, bs)
  fromBin {| a :+: b |} (0:bs)  = (Inl x, bs') where (x,bs') = fromBin bs
  fromBin {| a :+: b |} (1:bs)  = (Inr y, bs') where (y,bs') = fromBin bs
  fromBin {| a :*: b |} bs	= (x :*: y, bs'') where (x,bs' ) = fromBin bs
							(y,bs'') = fromBin bs'


class Tag a where
  nCons :: a -> Int
  nCons {| Unit |}    _ = 1
  nCons {| a :*: b |} _ = 1
  nCons {| a :+: b |} _ = nCons (bot::a) + nCons (bot::b)

  tag :: a -> Int
  tag {| Unit |}    _ 	    = 1
  tag {| a :*: b |} _ 	    = 1   
  tag {| a :+: b |} (Inl x) = tag x
  tag {| a :+: b |} (Inr y) = nCons (bot::a) + tag y
  
bot = bot

instance (Bin a, Bin b) => Bin (a,b)
instance Bin a => Bin [a]
instance Bin a => Bin (T a)

instance Bin Int where
  toBin x = [x]
  fromBin (x:xs) = (x,xs)

data T a = MkT a (T a) (T a) | Nil deriving Show

instance Tag Colour 
data Colour = Red | Blue | Green | Purple | White

t :: T Int
t = MkT 3 (MkT 6 Nil Nil) Nil

main = print (toBin t) >>
       print ((fromBin (toBin t))::(T Int,[Int])) >>
       print (tag Blue) >>
       print (tag White) >>
       print (nCons Red)

