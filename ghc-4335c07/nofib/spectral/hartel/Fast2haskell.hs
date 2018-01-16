{-# OPTIONS -fglasgow-exts #-}

import GHC.Prim;
import Data.Complex;
import Data.Array;
import GHC.Base;

type Complex_type = Complex Double;
type Array_type b = Array Int b;
type Assoc_type a = (Int,a);
type Descr_type = (Int,Int);

w2i x = word2Int# x;
i2w x = int2Word# x;

abortstr str = error ("abort:" ++ str);

delay x = abortstr "delay not implemented";

fix :: (x -> x) -> x;
fix f = fix_f where {fix_f = f fix_f};

force x = x; -- error "force not implemented"

iff :: Bool -> x -> x -> x;
iff b x y = if b then x else y;

iffrev :: x -> x -> Bool -> x;
iffrev y x b = if b then x else y;

miraseq :: x -> y -> y;
miraseq x y = seq_const y x;  -- x should be marked #STRICT
seq_const x y = x;

pair :: [x] -> Bool;
pair [] = False;
pair x = True;

entier :: Double -> Double;
entier x = fromIntegral (floor x);

land_i :: Int -> Int -> Int;
land_i (I# x) (I# y) = I# (w2i (and# (i2w x) (i2w y)));

lnot_i :: Int -> Int;
lnot_i (I# x) = I# (w2i (not# (i2w x)));

lor_i :: Int -> Int -> Int;
lor_i (I# x) (I# y) = I# (w2i (or# (i2w x) (i2w y)));

lshift_i :: Int -> Int -> Int;
lshift_i (I# x) (I# y) = I# (w2i (shiftL# (i2w x) y));

rshift_i :: Int -> Int -> Int;
rshift_i (I# x) (I# y) = I# (w2i (shiftRL# (i2w x) y));

write x = abortstr "write not implemented";

descr :: Int -> Int -> Descr_type;
descr l u = (l,u);

destr_update :: Array_type x -> Int -> x -> Array_type x;
destr_update ar i x = ar // [(i,x)];

indassoc :: Assoc_type x -> Int;
indassoc (i,v) = i;

lowbound :: Descr_type -> Int;
lowbound (l,u) = l;

tabulate :: (Int -> x) -> Descr_type -> Array_type x;
tabulate f (l,u) = array (l,u) [(i, f i) | i <- [l..u]];

upbound :: Descr_type -> Int;
upbound (l,u) = u;

update :: Array_type x -> Int -> x -> Array_type x;
update ar i x = ar // [(i,x)];

valassoc :: Assoc_type x -> x;
valassoc (i,v) = v;
