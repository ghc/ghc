module Bit where
import LogFun
import Signal

data Bit = Bot | WeakZero | WeakOne | Zero | One | Top
  deriving (Eq,Show{-was:Text-})

instance Static Bit where
  intToSig = intToSigBit
  sigToInt = sigToIntBit
  showStaticSig = showBit

instance Lattice Bit where
  bot = Bot
  top = Top
  weakZero = WeakZero
  weakOne = WeakOne
  lub = lubBit
  pass = passBit

instance Signal Bit where
  showSig = showBit
  initial = Zero
  zerO    = Zero
  one     = One
  tt1     = tt1Bit
  tt2     = tt2Bit

instance Log Bit where
  dumLog = Zero

tt1Bit :: TT1 -> Bit -> Bit
tt1Bit (a,b) =
  let p = intBit a
      q = intBit b
      f x = case x of
              Bot  -> Bot
              Zero -> p
              One  -> q
              Top  -> Top
  in f

tt2Bit :: TT2 -> Bit -> Bit -> Bit
tt2Bit (a,b,c,d) = f
  where p = intBit a
        q = intBit b
        r = intBit c
        s = intBit d
        f x y = case x of
                  Bot  ->     case y of
                                Bot      -> Bot
                                WeakZero -> Bot
                                WeakOne  -> Bot
                                Zero     -> Bot
                                One      -> Bot
                                Top      -> Top
                  WeakZero -> case y of
                                Bot      -> Bot
                                WeakZero -> p
                                WeakOne  -> q
                                Zero     -> p
                                One      -> q
                                Top      -> Top
                  WeakOne  -> case y of
                                Bot      -> Bot
                                WeakZero -> r
                                WeakOne  -> s
                                Zero     -> r
                                One      -> s
                                Top      -> Top
                  Zero     -> case y of
                                Bot      -> Bot
                                WeakZero -> p
                                WeakOne  -> q
                                Zero     -> p
                                One      -> q
                                Top      -> Top
                  One      -> case y of
                                Bot      -> Bot
                                WeakZero -> r
                                WeakOne  -> s
                                Zero     -> r
                                One      -> s
                                Top      -> Top
                  Top      -> case y of
                                Bot      -> Top
                                WeakZero -> Top
                                WeakOne  -> Top
                                Zero     -> Top
                                One      -> Top
                                Top      -> Top

lubBit :: Bit -> Bit -> Bit
lubBit a b =
  case a of
    Bot      -> case b of
                  Bot      -> Bot
                  WeakZero -> WeakZero
                  WeakOne  -> WeakOne
                  Zero     -> Zero
                  One      -> One
                  Top      -> Top
    WeakZero -> case b of
                  Bot      -> Zero
                  WeakZero -> WeakZero
                  WeakOne  -> Top
                  Zero     -> Zero
                  One      -> One
                  Top      -> Top
    WeakOne  -> case b of
                  Bot      -> WeakOne
                  WeakZero -> Top
                  WeakOne  -> WeakOne
                  Zero     -> Zero
                  One      -> One
                  Top      -> Top
    Zero     -> case b of
                  Bot      -> Zero
                  WeakZero -> Zero
                  WeakOne  -> Zero
                  Zero     -> Zero
                  One      -> Top
                  Top      -> Top
    One      -> case b of
                  Bot      -> One
                  WeakZero -> One
                  WeakOne  -> One
                  Zero     -> Top
                  One      -> One
                  Top      -> Top
    Top      -> case b of
                  Bot      -> Top
                  WeakZero -> Top
                  WeakOne  -> Top
                  Zero     -> Top
                  One      -> Top
                  Top      -> Top

showBit :: Bit -> String
showBit Bot      = "v"
showBit WeakZero = "z"
showBit WeakOne  = "o"
showBit Zero     = "0"
showBit One      = "1"
showBit Top      = "^"


intBit :: Int -> Bit
intBit 0 = Zero
intBit 1 = One
intBit x =
  error ("\nintBit received bad Int " ++ show x ++ ".\n")

intToSigBit :: Int -> Bit
intToSigBit i
  | i==0  =  Zero
  | i==1  =  One
  | i==8  =  Bot
  | i==9  =  Top

sigToIntBit :: Bit -> Int
sigToIntBit Zero = 0
sigToIntBit One  = 1
sigToIntBit Bot  = 8
sigToIntBit Top  = 9

passBit :: Bit -> Bit -> Bit
passBit c a =
  case c of
    Bot  -> Bot
    Zero -> Bot
    One  -> a
    Top  -> Top

instance Num Bit where
  (+) = or2
  (*) = and2
  a - b  = xor a b
  negate = inv
  abs    = error "abs not defined for Signals"
  signum = error "signum not defined for Signals"
  fromInteger = error "fromInteger not defined for Signals"

