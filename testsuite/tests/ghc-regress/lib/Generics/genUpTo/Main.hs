{-# OPTIONS -fglasgow-exts #-}

{-

This example illustrate test-set generation,
namely all terms of a given depth are generated.

-}

module Main where
import Data.Generics
import LittleLanguage


-- Generate all terms of a given depth
genUpTo :: Data a => Int -> [a]
genUpTo 0 = []
genUpTo d = result
   where
     -- Getting hold of the result (type)
     result = concat (map recurse cons)

     -- Retrieve constructors of the requested type
     cons :: [Constr]
     cons = dataTypeConstrs (dataTypeOf (head result))

     -- Find all terms headed by a specific Constr
     recurse :: Data a => Constr -> [a]
     recurse con = gmapM (\_ -> genUpTo (d-1)) 
                         (fromConstr con)

     -- We could also deal with primitive types easily.
     -- Then we had to use cons' instead of cons.
     --
     cons' :: [Constr]
     cons' = case dataTypeRep ty of
              AlgRep cons -> cons
              IntRep      -> [mkIntConstr ty 0]
              FloatRep    -> [mkIntConstr ty 0]
              StringRep   -> [mkStringConstr ty "foo"]
      where
        ty = dataTypeOf (head result)     


-- For silly tests
data T0 = T0 T1 T2 T3 deriving (Show, Typeable, Data)
data T1 = T1a | T1b   deriving (Show, Typeable, Data)
data T2 = T2a | T2b   deriving (Show, Typeable, Data)
data T3 = T3a | T3b   deriving (Show, Typeable, Data)

main = print $ (   genUpTo 0 :: [Id]
               , ( genUpTo 1 :: [Id]
               , ( genUpTo 2 :: [Id]
               , ( genUpTo 2 :: [T0]
               , ( genUpTo 3 :: [Prog]
               )))))

