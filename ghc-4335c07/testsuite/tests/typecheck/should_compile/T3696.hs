{-# OPTIONS -Wall #-}

module T3696 where

class C a where c :: a

instance C Int where c = 37

def = c

use :: Int
use = def
