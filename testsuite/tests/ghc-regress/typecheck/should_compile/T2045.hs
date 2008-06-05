{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Trac #2045
-- ghc -fhpc --make Vhdl.hs -o gencirc -Wall

module ShouleCompile where

writeDefinitions :: Generic b
                 => b -> IO ()
writeDefinitions out =
  do let define v s =
           case s of
             Bool True     -> port "vcc"  []
             Bool False    -> port "gnd"  []
             Inv x         -> port "inv"  [x]

             And []        -> define v (Bool True)
             And [x]       -> port "id"   [x]
             And [x,y]     -> port "and2" [x,y]
             And (x:xs)    -> define (w 0) (And xs)
                           >> define v (And [x,w 0])

             Or  []        -> define v (Bool False)
             Or  [x]       -> port "id"   [x]
             Or  [x,y]     -> port "or2"  [x,y]
             Or  (x:xs)    -> define (w 0) (Or xs)
                           >> define v (Or [x,w 0])

             Xor  []       -> define v (Bool False)
             Xor  [x]      -> port "id"   [x]
             Xor  [x,y]    -> port "xor2" [x,y]
             Xor  (x:xs)   -> define (w 0) (Or xs)
                           >> define (w 1) (Inv (w 0))
                           >> define (w 2) (And [x, w 1])

                           >> define (w 3) (Inv x)
                           >> define (w 4) (Xor xs)
                           >> define (w 5) (And [w 3, w 4])
                           >> define v     (Or [w 2, w 5])

             Multi a1 a2 a3 a4 -> multi a1 a2 a3 a4
           where
            w i = v ++ "_" ++ show i

            multi n "RAMB16_S18" opts args =
              do putStr $
                      "  "
                   ++ " : "
                   ++ "RAMB16_S18"
                   ++ "\ngeneric map ("
                   ++ opts
                   ++ mapTo "DOP" [0,1] (get 16 2 outs)
                   ++ mapTo "ADDR" [0..9] (get 0 10 args)
              where
                outs = map (\i -> "o" ++ show i ++ "_" ++ v) [1..n]

                get :: Int -> Int -> [a] -> [a]
                get n' m xs = take m (drop n' xs)

                mapTo s' (n':ns) (x:xs) = s' ++ "(" ++ show n' ++ ")"
                                          ++ " => " ++ x ++ ",\n"
                                          ++ mapTo s' ns xs
                mapTo _ _ _ = ""



            multi n "RAMB16_S18_S18" opts args =
              do putStr $
                      opts
                   ++ mapTo "DOA" [0..15] (get 0 16 outs)
                   ++ mapTo "DOB" [0..15] (get 18 16 outs)
                   ++ mapTo "DOPA" [0,1] (get 16 2 outs)
                   ++ mapTo "DOPB" [0,1] (get 34 2 outs)
                   ++ mapTo "ADDRA" [0..9] (get 0 10 args)
                   ++ mapTo "ADDRB" [0..9] (get 10 10 args)
                   ++ mapTo "DIA" [0..15] (get 20 16 args)
                   ++ mapTo "DIB" [0..15] (get 38 16 args)
                   ++ mapTo "DIPA" [0,1] (get 36 2 args)
                   ++ mapTo "DIPB" [0,1] (get 54 2 args)
                   ++ head (get 56 1 args)
                   ++ head (get 57 1 args)
              where
                outs = map (\i -> "o" ++ show i ++ "_" ++ v) [1..n]

                get :: Int -> Int -> [a] -> [a]
                get _ _ = id

                mapTo s' (n':ns) (x:xs) = s' ++ "(" ++ show n' ++ ")"
                                          ++ " => " ++ x ++ ",\n"
                                          ++ mapTo s' ns xs
                mapTo _ _ _ = ""
            multi _ _ _ _ = undefined

            port n args | n == "id" =
              do putStr $
                      "  "
                   ++ v ++ " <= " ++ (head args) ++ ";\n"

            port _ _ = undefined
     netlistIO define (struct out)
     return ()

netlistIO :: (v -> S v -> IO ()) -> f Symbol -> IO (f v)
netlistIO = undefined

data Struct a

class Generic a where
  struct    :: a -> Struct Symbol
  struct = undefined

instance Generic (Signal a)

data Signal a

data Symbol

data S s
  = Bool      Bool
  | Inv       s
  | And       [s]
  | Or        [s]
  | Xor       [s]
  | Multi    Int String String [s]

