{-# OPTIONS -fglasgow-exts #-}

{-

This module is in reply to an email by C. Barry Jay
received on March 15, and handled within hours. CBJ
raises the very interesting issue of higher-order patterns.
It turns out that some form of it is readily covered in
our setting.

-}

module Main where
import Data.Generics


-- Sample datatypes
data T1 = T1a Int | T1b Float
        deriving (Show, Typeable, Data)
data T2 = T2a T1 T2 | T2b
        deriving (Show, Typeable, Data)

-- Eliminate a constructor if feasible
elim' :: (Data y, Data x) => Constr -> y -> Maybe x
elim' c y = if toConstr y == c
                then unwrap y
                else Nothing


-- Unwrap a term; Return its single component
unwrap :: (Data y, Data x) => y -> Maybe x 
unwrap y = case gmapQ (Nothing `mkQ` Just) y of
             [Just x] -> Just x
             _ -> Nothing


-- Eliminate a constructor if feasible; 2nd try
elim :: forall x y. (Data y, Data x) => (x -> y) -> y -> Maybe x
elim c y = elim' (toConstr (c (undefined::x))) y


-- Visit a data structure
visitor :: (Data x, Data y, Data z)
        => (x -> y) -> (x -> x) -> z -> z
visitor c f = everywhere (mkT g)
  where
    g y = case elim c y of
            Just x  -> c (f x) 
            Nothing -> y


-- Main function for testing
main = print $   ( (elim' (toConstr t1a) t1a) :: Maybe Int
               , ( (elim' (toConstr t1a) t1b) :: Maybe Int
               , ( (elim  T1a t1a)            :: Maybe Int
               , ( (elim  T1a t1b)            :: Maybe Int
               , ( (visitor T1a ((+) 46) t2)  :: T2
               )))))
 where
   t1a = T1a 42
   t1b = T1b 3.14
   t2  = T2a t1a (T2a t1a T2b)
