module Signal where

import LogFun

class (Eq a, Show{-was:Text-} a, Num a) => Signal a where
  showSig :: a -> String

  zerO, one, initial :: a

  tt1 :: TT1 -> a -> a
  tt2 :: TT2 -> a -> a -> a

  con10, buf, inv, con11 :: a -> a

  con20, and2, nimp,  id21  :: a -> a -> a
  nimp', id22, xor,   or2   :: a -> a -> a
  nor2,  equ2, inv22, imp'  :: a -> a -> a
  inv21, imp,  nand2, con21 :: a -> a -> a
  and3,  or3,  nand3, nor3  :: a -> a -> a -> a
  and4,  or4,  nand4, nor4  :: a -> a -> a -> a -> a

  con10 = tt1 tt_con10
  buf   = tt1 tt_id
  inv   = tt1 tt_inv
  con11 = tt1 tt_con11

  con20 = tt2 tt_con20
  and2  = tt2 tt_and2
  nimp  = tt2 tt_nimp
  id21  = tt2 tt_id21
  nimp' = tt2 tt_nimp'
  id22  = tt2 tt_id22
  xor   = tt2 tt_xor
  or2   = tt2 tt_or2
  nor2  = tt2 tt_nor2
  equ2  = tt2 tt_equ2
  inv22 = tt2 tt_inv22
  imp'  = tt2 tt_imp'
  inv21 = tt2 tt_inv21
  imp   = tt2 tt_imp
  nand2 = tt2 tt_nand2
  con21 = tt2 tt_con21

  and3  a b c = a*b*c
  or3   a b c = a+b+c
  nand3 a b c = nand2 a (nand2 b c)
  nor3  a b c = nor2 a (nor2 b c)

  and4  a b c d = (a*b)*(c*d)
  or4   a b c d = (a+b)+(c+d)
  nand4 a b c d = nand2 (nand2 a b) (nand2 c d)
  nor4  a b c d = nor2 (nor2 a b) (nor2 c d)

class (Signal a) => Lattice a where
  bot, top, weakZero, weakOne :: a
  lub  :: a -> a -> a
  pass :: a -> a -> a

class (Signal a) => Static a where
  intToSig :: Int -> a
  sigToInt :: a -> Int
  showStaticSig :: a -> String

class (Signal a) => Dynamic a where
  latch, dff :: a -> a

class (Lattice a, Static a) => Log a where
  dumLog :: a

class (Lattice a, Dynamic a) => Sig a where
  dumSig :: a

data Stream a = Snil | Scons a (Stream a)  deriving (Eq,Show{-was:Text-})

shead :: Stream a -> a
shead (Scons x xs) = x

stail :: Stream a -> Stream a
stail (Scons x xs) = xs

snull :: Stream a -> Bool
snull Snil = True
snull (Scons x xs) = False

smap :: (a->b) -> Stream a -> Stream b
smap f Snil = Snil
smap f (Scons x xs) = Scons (f x) (smap f xs)

stake, sdrop :: Int -> Stream a -> Stream a

stake 0 xs = xs
--should be: stake (i+1) (Scons x xs) = Scons x (stake i xs)
stake i (Scons x xs) | i < 0     = error "Signal.stake: < 0"
		     | otherwise = Scons x (stake (i-1) xs)

sdrop 0 xs = xs
--should be:sdrop (i+1) (Scons x xs) = sdrop i xs
sdrop i (Scons x xs) | i < 0	 = error "Signal.sdrop: < 0"
		     | otherwise = sdrop i xs

smap2 :: (a->b->c) -> Stream a -> Stream b -> Stream c
smap2 f as bs =
  case as of
    Snil -> Snil
    Scons a as' ->
      case bs of
        Snil -> Snil
        Scons b bs' -> Scons (f a b) (smap2 f as' bs')

srepeat :: (Static a) => a -> Stream a
srepeat x = xs where xs = Scons x xs

stream :: [a] -> Stream a
stream [] = Snil
stream (x:xs) = Scons x (stream xs)

instance (Signal a, Static a) => Dynamic (Stream a) where
  latch xs = Scons initial xs
  dff xs = Scons initial xs

instance (Lattice a, Static a) => Lattice (Stream a) where
  bot      = srepeat bot
  top      = srepeat top
  weakZero = srepeat weakZero
  weakOne  = srepeat weakOne
  lub      = smap2 lub
  pass     = smap2 pass

instance (Signal a, Static a) => Signal (Stream a) where
  zerO = srepeat zerO
  one  = srepeat one
  tt1  = smap . tt1
  tt2  = smap2 . tt2

instance (Lattice a, Static a) => Sig (Stream a) where
  dumSig = bot  -- ??? shouldn't be necessary, check compiler

instance (Static a) => Num (Stream a) where
  (+) = or2
  (*) = and2
  a - b  = xor a b
  negate = inv
  abs    = error "abs not defined for Signals"
  signum = error "signum not defined for Signals"
  fromInteger = error "fromInteger not defined for Signals"

