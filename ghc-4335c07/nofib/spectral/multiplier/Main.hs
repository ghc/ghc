-------------------------------------------------------------------
-- Binary Multiplier  --- Circuit Specification and Simulation
-- John O'Donnell, University of Glasgow. jtod@dcs.glasgow.ac.uk
-- Copyright (c) 1992 by John T. O'Donnell
-- version July 1992

-------------------------------------------------------------------
{- Introduction

This program demonstrates some of my work in using functional
programming for digital circuit design.  It works with the hbc
compiler, and if you comment out the definitions of 'Main' and
'main' it also works with gofer.

The program isn't polished -- I've been experimenting with several
variations, and some of the function definitions are inelegant or
inefficient.

This source file 'mult.hs' is written in Haskell and contains one
module 'Main'.  The program doesn't read any input; it just prints
a string on stdout.  To run it with hbc:

  (1) set parameters in function 'go'
      (wordsize, verbose, limit, xs)
  (2) hbc mult.hs   (produces a.out)
  (3) a.out         (no input, writes to stdout)

-}
-------------------------------------------------------------------
{- Parameters

These parameters defined in "go" control the simulation:

** wordsize
      How many bits wide the input words to be multiplied are.
      The product is 2*wordsize bits wide.
** limit
      The number of clock cycles to run the simulation.
** verbose
      If True, print lots of output.  If False, do the simulation
      but print hardly anything.
** width
      Field width in characters for integer output.
      (Used only if verbose)
** xs
      A list of pairs of natural numbers to be multiplied.
      These should be expressible in 'wordsize' bits, and the
      products should be expressible in 2*wordsize bits.

These parameter values work nicely:
    wordsize = 16	-- input size; product is twice this size
    limit = 2000	-- how many clock cycles to run
    verbose = True	-- want lots of output?
    width = 10		-- output Int field size (verbose only)
    xs =		-- pairs of integers to be multiplied
      [(2+3*i, 11+2*i) | i <- [1..]]

The verbose output contains one line per clock cycle:
    cycle.  start  a    b    ==>   ready   regA   regB   prod

-}

-------------------------------------------------------------------
{- The circuit multiplies pairs (a,b) of naturals, producing a*b

Inputs to the circuit
   start :: B
   a :: W
   b :: W

Outputs from the circuit
   ready :: B
   regA  :: W
   regB :: W
   regP :: W

Whenever 'start' is 1 the circuit saves its inputs 'a' and 'b' into
its local registers 'regA' and 'regB', and starts multiplying them
using the ordinary shift and add algorithm.  The result is
accumulated in the local register 'regP'.  While the multiplication
is in progress, 'ready' is 0 and the registers show the current
state of the circuit.  When the multiplication is finished, 'regP'
contains the product and 'ready' becomes 1.  The number of cycles
it takes to finish a multiplication depends on the values of the
inputs.

-}

-------------------------------------------------------------------

module Main where

import System.Environment

main = do
  (n:_) <- getArgs
  putStr (go (read n :: Int))

-------------------------------------------------------------------
-- Definition of the output and the simulation parameters

go wordsize =
  let
    limit = 2000	-- how many clock cycles to run
    verbose = True	-- want lots of output?
    width = 10		-- output Int field size (verbose only)
    xs =		-- pairs of integers to be multiplied
      [(2+3*i, 11+2*i) | i <- [1..]]
  in
    "Binary multiplier circuit simulation\n"
    ++ (if verbose then traceMult width else runMult)
         wordsize limit xs

-------------------------------------------------------------------
-- Two simulation drivers

-- 'traceMult' gives full trace information.  Use it to see how
-- the multiplier works.

traceMult :: Int -> Int -> Int -> [(Int,Int)] -> String
traceMult width wordsize limit xs =
  format limit
    [fmtInt 5 [0..], fmtStr ". ",
     fmtB start, fmtW width as, fmtW width bs, fmtStr "  ==> ",
     fmtB ready, fmtW width ra, fmtW width rb, fmtW width prod,
     fmtStr "\n"]
  where (as,bs,ready,ra,rb,prod) = multsys wordsize xs
        start = ready

-- 'runMult' runs the simulation for "limit" cycles and just prints
-- the number of multiplications that were performed.  Use this
-- to measure simulation time without counting the time required
-- to format the output.  ??? This needs some work -- I'm not sure
-- that all the simulation work is actually performed!

runMult :: Int -> Int -> [(Int,Int)] -> String
runMult wordsize limit xs = show dummy ++ "\n"
  where (as,bs,ready,ra,rb,prod) = multsys wordsize xs
        dummy = (sum (take limit ready)) :: Int

-------------------------------------------------------------------
-- Interface to the multiplier

-- The multiplier circuit needs an interface that monitors the
-- 'ready' output and sets the inputs.  This interface also handles
-- conversions between integers and words.

multsys :: Int -> [(Int,Int)] -> (W,W,B,W,W,W)
multsys wordsize xs = (as,bs,ready,ra,rb,prod)
  where (ready, ra, rb,  prod) = multiplier wordsize start as bs
        start = ready
        as  = f (map fst xs)
        bs  = f (map snd xs)
        f :: [Int] -> W
        f as = ntrans wordsize (map (ibits wordsize) (g start as))
        g :: B -> [Int] -> [Int]
        g [] xs = []
        g st [] = []
        g (0:sts) xs = 0 : g sts xs
        g (1:sts) (x:xs) = x : g sts xs

-------------------------------------------------------------------
-- The multiplier circuit specification

multiplier :: Int -> B -> W -> W -> (B,W,W,W)

multiplier k start a b = (ready, regA, regB, regP)
  where
    regP = wlat (2*k) (wmux1 (2*k) start sum (rept (2*k) zerO))
    (ovfl,sum) = add (2*k) regP (wmux1 (2*k) lsbB
                   (rept (2*k) zerO) regA) zerO
    regA = wlat (2*k) (wmux1 (2*k) start (shl (2*k) regA)
                   (rept k zerO ++ a))
    regB = wlat k (wmux1 k start (shr k regB) b)
    lsbB = head (drop (k-1) regB)
    ready = or2 (regIs0 (2*k) regA) (regIs0 k regB)

-------------------------------------------------------------------
-- Comparator

regIs0 :: Int -> W -> B
regIs0 k xs = wideAnd (map inv xs)

-------------------------------------------------------------------
-- Combinational shifters

shl :: Int -> W -> W
shl k xs = drop 1 xs ++ [zerO]

shr :: Int -> W -> W
shr k xs = [zerO] ++ take (k-1) xs

-------------------------------------------------------------------
-- Adder

add :: Int -> W -> W -> B -> (B,W)
add 0 xs ys cin = (cin,[])
--should be:add (k+1) (x:xs) (y:ys) cin = (cout,s:ss)
add k (x:xs) (y:ys) cin
  | k < 0     = error "Main.add < 0"
  | otherwise = (cout,s:ss)
  where
  (cout,s) = fulladd x y c
  (c,ss) = add (k-1) xs ys cin

halfadd :: B -> B -> (B,B)
halfadd x y = (and2 x y, xor x y)

fulladd :: B -> B -> B -> (B,B)
fulladd a b c = (or2 w y, z)
  where (w,x) = halfadd a b
        (y,z) = halfadd x c

-------------------------------------------------------------------
-- Multiplexors and demultiplexors

bmux1 :: B -> B -> B -> B
bmux1 c a b = or2 (and2 (inv c) a) (and2 c b)

wmux1 :: Int -> B -> W -> W -> W
wmux1 k a = word21 k (bmux1 a)

bdemux1 :: B -> B -> (B,B)
bdemux1 c a = (and2 (inv c) a, and2 c a)

bdemux :: Int -> [B] -> B -> [B]
bdemux 0 [] x = [x]
--should be:bdemux (n+1) as x = bdemux n (tail as) p ++ bdemux n (tail as) q
bdemux n as x 
 | n < 0 = error "bdemux; n < 0"
 | otherwise = let n' = n-1 in
  bdemux n' (tail as) p ++ bdemux n' (tail as) q
  where (p,q) = bdemux1 (head as) x

-------------------------------------------------------------------
-- Registers

breg :: B -> B -> B
breg sto a = x
  where x = latch (bmux1 sto x a)

wreg :: Int -> B -> [B] -> [B]
wreg 0 sto [] = []
wreg n sto (x:xs) =
  breg sto x : wreg (n-1) sto xs

wlat :: Int -> [B] -> [B]
wlat 0 xs = []
--should be:wlat (k+1) (x:xs) = latch x : wlat k xs
wlat k (x:xs) | k < 0 = error "wlat"
	      | otherwise = latch x : wlat (k-1) xs

-------------------------------------------------------------------
-- Primitive components
                                 
latch :: B -> B
latch a = 0:a

zerO, one :: B
zerO = 0:zerO
one  = 1:one

inv = lift11 forceBit f
  where f :: Bit -> Bit
        f 0 = 1
        f 1 = 0

and2 = lift21 forceBit f
  where f :: Bit -> Bit -> Bit
        f 0 0 = 0
        f 0 1 = 0
        f 1 0 = 0
        f 1 1 = 1

nand2 = lift21 forceBit f
  where f :: Bit -> Bit -> Bit
        f 0 0 = 1
        f 0 1 = 1
        f 1 0 = 1
        f 1 1 = 0

or2 = lift21 forceBit f
  where f :: Bit -> Bit -> Bit
        f 0 0 = 0
        f 0 1 = 1
        f 1 0 = 1
        f 1 1 = 1

nor2 = lift21 forceBit f
  where f :: Bit -> Bit -> Bit
        f 0 0 = 1
        f 0 1 = 0
        f 1 0 = 0
        f 1 1 = 0

or3 = lift31 forceBit f
  where f :: Bit -> Bit -> Bit -> Bit
        f 0 0 0 = 0
        f 0 0 1 = 1
        f 0 1 0 = 1
        f 0 1 1 = 1
        f 1 0 0 = 1
        f 1 0 1 = 1
        f 1 1 0 = 1
        f 1 1 1 = 1

xor = lift21 forceBit f
  where f :: Bit -> Bit -> Bit
        f 0 0 = 0
        f 0 1 = 1
        f 1 0 = 1
        f 1 1 = 0

-------------------------------------------------------------------
-- Wide gates

wideGate f [x] = x
wideGate f xs =
  f (wideGate f (take i xs))
    (wideGate f (drop i xs))
  where i = length xs `div` 2

wideAnd xs  = wideGate and2 xs
wideNand xs = wideGate nand2 xs
wideOr xs   = wideGate or2 xs
wideNor xs  = wideGate nor2 xs

-------------------------------------------------------------------
-- Auxiliary definitions

type Bit = Int
type B = [Bit]
type W = [B]

forceBit :: Bit->Bool
forceBit x = (x==0)

headstrict :: (a->Bool) -> [a] -> [a]
headstrict force [] = []
headstrict force xs = if force (head xs) then xs else xs

pairstrict :: (a->Bool) -> (b->Bool) -> ([a],[b]) -> ([a],[b])
pairstrict force1 force2 p =
  if force1 (head x)
     then if force2 (head y) then p else p
     else if force2 (head y) then p else p
  where (x,y) = p

lift11 force f [] = []
lift11 force f (x:xs) = headstrict force (f x : lift11 force f xs)

{-
lift21 force f [] zs = []
lift21 force f ys [] = []
lift21 force f (y:ys) (z:zs) =
  headstrict force (f y z : lift21 force f ys zs)
-}

--lift21 force f [] zs = []
--lift21 force f ys [] = []
lift21 force f (y:ys) (z:zs) =
   (f y z : lift21 force f ys zs) -- ??? space leak here?

lift31 force f [] ys zs = []
lift31 force f xs [] zs = []
lift31 force f xs ys [] = []
lift31 force f (x:xs) (y:ys) (z:zs) =
  headstrict force (f x y z : lift31 force f xs ys zs)

lift41 force f [] xs ys zs = []
lift41 force f ws [] ys zs = []
lift41 force f ws xs [] zs = []
lift41 force f ws xs ys [] = []
lift41 force f (w:ws) (x:xs) (y:ys) (z:zs) =
  headstrict force (f w x y z : lift41 force f ws xs ys zs)

lift22 force1 force2 f [] ys = ([],[])
lift22 force1 force2 f xs [] = ([],[])
lift22 force1 force2 f (x:xs) (y:ys) =
  pairstrict force1 force2 (a:as, b:bs)
  where (a,b) = f x y
        (as,bs) = lift22 force1 force2 f xs ys

-------------------------------------------------------------------
-- Words

word11 :: Int -> (a->b) -> [a] -> [b]
word11 0 f as = []
word11 k f as = f (head as) : word11 (k-1) f (tail as)

word21 :: Int -> (a->b->c) -> [a] -> [b] -> [c]
word21 0 f as bs = []
word21 k f as bs =
  f (head as) (head bs) : word21 (k-1) f (tail as) (tail bs)

word31 :: Int -> (a->b->c->d) -> [a] -> [b] -> [c] -> [d]
word31 0 f as bs cs = []
word31 k f as bs cs =
  f (head as) (head bs) (head cs) :
    word31 (k-1) f (tail as) (tail bs) (tail cs)

word12 :: Int -> (a->(b,c)) -> [a] -> ([b],[c])
word12 0 f as = ([],[])
word12 k f as = (b:bs,c:cs)
  where (b,c) = f (head as)
        (bs,cs) = word12 (k-1) f (tail as)

word22 :: Int -> (a->b->(c,d)) -> [a] -> [b] -> ([c],[d])
word22 0 f as bs = ([],[])
word22 k f as bs = (c:cs,d:ds)
  where (c,d) = f (head as) (head bs)
        (cs,ds) = word22 (k-1) f (tail as) (tail bs)

-------------------------------------------------------------------
-- Conversions

shoInt :: Int -> String
shoInt n = show n

trans :: [[a]] -> [[a]]
trans xs =
  if or (map null xs)
    then []
    else map head xs : trans (map tail xs)

ntrans 0 xs = []
ntrans i xs = map head xs : ntrans (i-1) (map tail xs)

dec :: Int -> Int -> String
dec k n =
  if i<k then (rept (k-i) ' ') ++ xs else xs
  where xs = show n
        i = length xs

ibits :: Int -> Int -> [Int]
ibits n i = reverse (f_ibits n i)
  where f_ibits 0 i = []
        f_ibits n i = i `mod` 2 : f_ibits (n-1) (i `div` 2)

{- bitsi converts a binary number represented by a list of bits
into an integer. -}

bitsi :: [Int] -> Int
bitsi = f_bitsi 0
  where f_bitsi i [] = i
        f_bitsi i (b:bs) = f_bitsi (2*i+b) bs

-- These functions lift the integer--bits conversions to streams.

intrep :: [B] -> [Int]
intrep bs = map bitsi (trans bs)

bitrep :: Int -> [Int] -> [B]
bitrep n = ntrans n . map (ibits n)

-------------------------------------------------------------------
-- Formatting

rept :: Int -> a -> [a]
rept 0 x = []
rept i x = x : rept (i-1) x

mksepline c = "\n" ++ rept 79 c ++ "\n"
sepline = mksepline '-'
bigsepline = mksepline '='
   
format :: Int -> [[[a]]] -> [a]
format limit = concat . take limit . map concat . trans

--fmtW :: Int -> W ->
fmtW i xs = fmtDec i (intrep xs)

-- fmtDec  width of field
fmtDec :: Int -> [Int] -> [String]
fmtDec w = map (dec w)

fmtB :: B -> [String]
fmtB = map (dec 1)

fmtInt :: Int -> [Int] -> [String]
fmtInt i = map (dec i)

fmtFld :: (Int->Bit->String) -> Int -> [B] -> [String]
fmtFld f i xs = map (f i) (intrep xs)

fmtList :: (a->String) -> [[a]] -> [String]
fmtList f xs = map (g . concat . map f) xs
  where g cs = cs ++ "  "

fmtStr :: String -> [String]
fmtStr s = s : fmtStr s

{- when takes a ready signal xs and an arbitrary signal ys,
returning the value on the ys signal at the first cycle that xs is
1. -}

when :: B -> W -> [Int]
when (0:xs) w = when xs (map tail w)
when (1:xs) w = map head w

-------------------------------------------------------------------
