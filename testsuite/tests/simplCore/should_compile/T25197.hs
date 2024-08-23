{-# LANGUAGE TemplateHaskell #-}

module T25197 where

import T25197_TH
import GHC.Exts

{-
This test applies a large statically known data structure to a function with
a SPEC argument, forcing the function to be specialised for the argument.
However when the complete structure of the argument is not statically known,
or as here the leaves of the structures are primitive literals for which we do
not specialize this results in a specialized function that can take hundreds of
arguments.

Typically this is not intended, therefore we use a limit on the number of
arguments for specializations. As at some point this sort of specialization
comes with a heavy compile time cost. However we allow users to specify this
limit just in case they really depend on this sort of specialization.
-}

foo :: [a] -> Int
foo = go SPEC
  where
    go :: SPEC -> [a] -> Int
    go s []     = s `seq` 0
    go s (_:xs) = 1 + go s xs

main :: IO ()
main = print $ foo $(gen 1000)
