{-# OPTIONS -fglasgow-exts #-}

{-

A very, very simple example: "extract all Ints from a tree of Ints".
The text book approach is to write a generalised fold for that. One
can also turn the Tree datatype into functorial style and then write a
Functor instance for the functorial datatype including a definition of
fmap. (The original Tree datatype can be related to the functorial
version by the usual injection and projection.)

You can scrap all such boilerplate by using a traversal scheme based
on gmap combinators as illustrated below. To get it a little more
interesting, we use a datatype Tree with not just a case for leafs and
fork trees, but we also add a case for trees with a weight.

For completeness' sake, we mention that the fmap/generalised fold
approach differs from the gmap approach in some details. Most notably,
the gmap approach does not generally facilitate the identification of
term components that relate to the type parameter of a parameterised
datatype. The consequence of this is illustrated below as well.
Sec. 6.3 in "Scrap Your Boilerplate ..." discusses such `type
distinctions' as well.

-}

module Main where


-- Enable "ScrapYourBoilerplate"
import Data.Generics


-- A parameterised datatype for binary trees with data at the leafs
data (Data a, Data w) =>
     Tree a w = Leaf a
              | Fork (Tree a w) (Tree a w)
              | WithWeight (Tree a w) w  
       deriving (Typeable, Data)


-- A typical tree
mytree :: Tree Int Int
mytree = Fork (WithWeight (Leaf 42) 1)
              (WithWeight (Fork (Leaf 88) (Leaf 37)) 2)


-- Print everything like an Int in mytree
-- In fact, we show two attempts:
--   1. print really just everything like an Int
--   2. print everything wrapped with Leaf
-- So (1.) confuses leafs and weights whereas (2.) does not.
-- 
main = print $ ( listify (\(_::Int) -> True)         mytree
               , everything (++) ([] `mkQ` fromLeaf) mytree
               )
  where
    fromLeaf :: Tree Int Int -> [Int]
    fromLeaf (Leaf x) = [x]
    fromLeaf _ = []
