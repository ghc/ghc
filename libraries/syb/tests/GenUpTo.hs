{-# OPTIONS -fglasgow-exts #-}

module GenUpTo (tests) where

{-

This example illustrate test-set generation,
namely all terms of a given depth are generated.

-}

import Test.Tasty.HUnit

import Data.Generics


{-

The following datatypes comprise the abstract syntax of a simple
imperative language. Some provisions are such that the discussion
of test-set generation is simplified. In particular, we do not
consider anything but monomorphic *data*types --- no primitive
types, no tuples, ...

-}

data Prog = Prog Dec Stat
            deriving (Show, Eq, Typeable, Data)

data Dec  = Nodec
          | Ondec Id Type
          | Manydecs Dec Dec
            deriving (Show, Eq, Typeable, Data)

data Id = A | B
          deriving (Show, Eq, Typeable, Data)

data Type = Int | Bool
            deriving (Show, Eq, Typeable, Data)

data Stat = Noop
          | Assign Id Exp
          | Seq Stat Stat
            deriving (Show, Eq, Typeable, Data)

data Exp = Zero
         | Succ Exp
           deriving (Show, Eq, Typeable, Data)


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
              IntRep      -> [mkIntegralConstr ty 0]
              FloatRep    -> [mkIntegralConstr ty 0]
              CharRep     -> [mkCharConstr ty 'x']
      where
        ty = dataTypeOf (head result)


-- For silly tests
data T0 = T0 T1 T2 T3 deriving (Show, Eq, Typeable, Data)
data T1 = T1a | T1b   deriving (Show, Eq, Typeable, Data)
data T2 = T2a | T2b   deriving (Show, Eq, Typeable, Data)
data T3 = T3a | T3b   deriving (Show, Eq, Typeable, Data)

tests = (   genUpTo 0 :: [Id]
        , ( genUpTo 1 :: [Id]
        , ( genUpTo 2 :: [Id]
        , ( genUpTo 2 :: [T0]
        , ( genUpTo 3 :: [Prog]
        ))))) @=? output

output = ([],([A,B],([A,B],([T0 T1a T2a T3a,T0 T1a T2a T3b,T0 T1a T2b T3a,T0 T1a T2b T3b,T0 T1b T2a T3a,T0 T1b T2a T3b,T0 T1b T2b T3a,T0 T1b T2b T3b],[Prog Nodec Noop,Prog Nodec (Assign A Zero),Prog Nodec (Assign B Zero),Prog Nodec (Seq Noop Noop),Prog (Ondec A Int) Noop,Prog (Ondec A Int) (Assign A Zero),Prog (Ondec A Int) (Assign B Zero),Prog (Ondec A Int) (Seq Noop Noop),Prog (Ondec A Bool) Noop,Prog (Ondec A Bool) (Assign A Zero),Prog (Ondec A Bool) (Assign B Zero),Prog (Ondec A Bool) (Seq Noop Noop),Prog (Ondec B Int) Noop,Prog (Ondec B Int) (Assign A Zero),Prog (Ondec B Int) (Assign B Zero),Prog (Ondec B Int) (Seq Noop Noop),Prog (Ondec B Bool) Noop,Prog (Ondec B Bool) (Assign A Zero),Prog (Ondec B Bool) (Assign B Zero),Prog (Ondec B Bool) (Seq Noop Noop),Prog (Manydecs Nodec Nodec) Noop,Prog (Manydecs Nodec Nodec) (Assign A Zero),Prog (Manydecs Nodec Nodec) (Assign B Zero),Prog (Manydecs Nodec Nodec) (Seq Noop Noop)]))))
