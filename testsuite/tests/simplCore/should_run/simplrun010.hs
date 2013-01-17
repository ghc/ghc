{-# LANGUAGE ForeignFunctionInterface, MagicHash, UnboxedTuples #-}

-- From trac #1947
-- Should fail with heap exhaustion
-- See notes below with "Infinite loop here".

module Main(main) where

import System.IO.Unsafe
import System.IO
import System.Environment
import System.Exit
import Foreign.C.Types
import Data.Char(ord,chr)


-- low level imports
import GHC.Base                 (realWorld#)
import GHC.IO                   (IO(IO), unIO, unsafePerformIO)
import GHC.Prim                 (State#,RealWorld)


-- FFI replacements for Haskell stuff
foreign import ccall unsafe "stdio.h getchar" getchar :: IO CInt
foreign import ccall unsafe "ctype.h iswspace" isspace :: CInt -> CInt


skipCAF :: State# RealWorld -> a -> a
skipCAF _ x = x


-- IO Subsystem
-- Unboxed IO is more efficient, but requires a certain level of
-- optimisation, so provide a BOXED_IO fallback

data RW_Box = RW_Box (State# RealWorld)
type RW_Pair a = (RW_Box, a)

fromIO :: IO a -> (RW_Box -> RW_Pair a)
fromIO a (RW_Box r) = case unIO a r of (# r, x #) -> (RW_Box r, x)

toIO :: (RW_Box -> RW_Pair a) -> IO a
toIO f = IO $ \r -> case f (RW_Box r) of (RW_Box r, x) -> (# r, x #)

-- IO functions not dependent on the IO primitives
main :: IO ()
main = toIO main_generated

typeRealWorld :: RW_Box -> RW_Box
typeRealWorld x = x

overlay_get_char :: RW_Box -> RW_Pair Int
overlay_get_char = fromIO $ do
    c <- getchar
    return $ fromIntegral c

system_IO_hPutChar :: Handle -> Int -> RW_Box -> RW_Pair ()
system_IO_hPutChar h c = fromIO $ hPutChar h (chr c)

overlay_errorIO :: [Int] -> RW_Box -> RW_Pair a
overlay_errorIO x r = case fromIO (putStrLn ("ERROR: " ++ map chr x)) r of
                           (r, _) -> fromIO exitFailure r

system_Environment_getArgs :: RW_Box -> RW_Pair [[Int]]
system_Environment_getArgs r = case (fromIO getArgs) r of
                                    (r, s) -> (r, map str_ s)

overlay_supero_wrap x = x


-- Primitives
prelude_seq = seq

prelude_error x = error (map chr x)

aDD_W = (+) :: Int -> Int -> Int
mUL_W = (*) :: Int -> Int -> Int
sUB_W = (-) :: Int -> Int -> Int
eQ_W = (==) :: Int -> Int -> Bool
nE_W = (/=) :: Int -> Int -> Bool
gT_W = (>) :: Int -> Int -> Bool
gE_W = (>=) :: Int -> Int -> Bool
lT_W = (<) :: Int -> Int -> Bool
lE_W = (<=) :: Int -> Int -> Bool
qUOT = quot :: Int -> Int -> Int
rEM = rem :: Int -> Int -> Int
nEG_W = negate :: Int -> Int
yHC_Primitive_primIntAbs = abs :: Int -> Int
yHC_Primitive_primIntSignum = signum :: Int -> Int
yHC_Primitive_primIntegerAdd = (+) :: Integer -> Integer -> Integer
yHC_Primitive_primIntegerEq = (==) :: Integer -> Integer -> Bool
yHC_Primitive_primIntegerFromInt = toInteger :: Int -> Integer
yHC_Primitive_primIntegerGe = (>=) :: Integer -> Integer -> Bool
yHC_Primitive_primIntegerGt = (>) :: Integer -> Integer -> Bool
yHC_Primitive_primIntegerLe = (<=) :: Integer -> Integer -> Bool
yHC_Primitive_primIntegerMul = (*) :: Integer -> Integer -> Integer
yHC_Primitive_primIntegerNe = (/=) :: Integer -> Integer -> Bool
yHC_Primitive_primIntegerNeg = negate :: Integer -> Integer
yHC_Primitive_primIntegerQuot = quot :: Integer -> Integer -> Integer
yHC_Primitive_primIntegerQuotRem = quotRem :: Integer -> Integer -> (Integer, Integer)
yHC_Primitive_primIntegerRem = rem :: Integer -> Integer -> Integer
yHC_Primitive_primIntFromInteger = fromInteger :: Integer -> Int
yHC_Primitive_primIntegerLt = (<) :: Integer -> Integer -> Bool
yHC_Primitive_primIntegerSub = (-) :: Integer -> Integer -> Integer

aDD_D = (+) :: Double -> Double -> Double
sUB_D = (-) :: Double -> Double -> Double
lT_D = (<) :: Double -> Double -> Bool
lE_D = (<=) :: Double -> Double -> Bool
gT_D = (>) :: Double -> Double -> Bool
gE_D = (>=) :: Double -> Double -> Bool
eQ_D = (==) :: Double -> Double -> Bool
mUL_D = (*) :: Double -> Double -> Double
nEG_D = (negate) :: Double -> Double
nE_D = (/=) :: Double -> Double -> Bool
sLASH_D = (/) :: Double -> Double -> Double
yHC_Primitive_primDecodeDouble = decodeFloat :: Double -> (Integer,Int)
yHC_Primitive_primDoubleACos = acos :: Double -> Double
yHC_Primitive_primDoubleASin = asin :: Double -> Double
yHC_Primitive_primDoubleATan = atan :: Double -> Double
yHC_Primitive_primDoubleAbs = abs :: Double -> Double
yHC_Primitive_primDoubleCos = cos :: Double -> Double
yHC_Primitive_primDoubleExp = exp :: Double -> Double
yHC_Primitive_primDoubleFromInteger = fromInteger :: Integer -> Double
yHC_Primitive_primDoubleLog = log :: Double -> Double
yHC_Primitive_primDoublePow = (**) :: Double -> Double -> Double
yHC_Primitive_primDoubleSignum = signum :: Double -> Double
yHC_Primitive_primDoubleSin = sin :: Double -> Double
yHC_Primitive_primDoubleSqrt = sqrt :: Double -> Double
yHC_Primitive_primDoubleTan = tan :: Double -> Double
yHC_Primitive_primEncodeDouble = encodeFloat :: Integer -> Int -> Double




-- things which Yhc decides should be hopelessly slow
prelude_Int_Integral_mod = mod :: Int -> Int -> Int
prelude_Integer_Integral_div = div :: Integer -> Integer -> Integer
prelude_Integer_Integral_mod = mod :: Integer -> Integer -> Integer
prelude_Integer_Num_signum = signum :: Integer -> Integer
prelude_Integer_Num_abs = abs :: Integer -> Integer


int_ x = x :: Int
chr_ x = ord x
str_ x = map chr_ x


system_IO_stdin = stdin
system_IO_stdout = stdout

data_Char_isSpace :: Int -> Bool
data_Char_isSpace c = isspace (toEnum c) /= 0



type ReadsPrec a = Int -> [Int] -> [(a,[Int])]


prelude_Int_Read_readsPrec :: ReadsPrec Int
prelude_Int_Read_readsPrec p s = [(a, str_ b) | (a,b) <- readsPrec p (map chr s)]
prelude_Int_Read_readList = undefined

prelude_Integer_Read_readsPrec :: ReadsPrec Integer
prelude_Integer_Read_readsPrec p s = [(a, str_ b) | (a,b) <- readsPrec p (map chr s)]
prelude_Integer_Read_readList = undefined

prelude_Double_Read_readsPrec :: ReadsPrec Double
prelude_Double_Read_readsPrec p s = [(a, str_ b) | (a,b) <- readsPrec p (map chr s)]
prelude_Double_Read_readList = undefined

prelude_Char_Read_readsPrec :: ReadsPrec Int
prelude_Char_Read_readsPrec p s = [(chr_ (a :: Char), str_ b) | (a,b) <- readsPrec p (map chr s)]

prelude_Char_Show_showList :: [Int] -> [Int] -> [Int]
prelude_Char_Show_showList value rest = str_ (show (map chr value)) ++ rest

prelude_Char_Show_showsPrec :: Int -> Int -> [Int] -> [Int]
prelude_Char_Show_showsPrec prec i rest = str_ (showsPrec prec (chr i) []) ++ rest

prelude_Int_Show_showsPrec :: Int -> Int -> [Int] -> [Int]
prelude_Int_Show_showsPrec prec i rest = str_ (showsPrec prec i []) ++ rest

prelude_Integer_Show_showsPrec :: Int -> Integer -> [Int] -> [Int]
prelude_Integer_Show_showsPrec prec i rest = str_ (showsPrec prec i []) ++ rest

prelude_Double_Show_showsPrec :: Int -> Double -> [Int] -> [Int]
prelude_Double_Show_showsPrec prec i rest = str_ (showsPrec prec i []) ++ rest


prelude_'amp'amp27 v1 v2 =
    case (data_Char_isSpace v1) of
        True ->
            case v2 of
                [] -> True
                (:) v4 v5 -> prelude_'amp'amp27 v4 v5
        False -> False

prelude_LAMBDA22 v1 v2 =
    case v1 of
        (,) v267 v268 ->
            case v268 of
                [] -> prelude_LAMBDA24 v267 v2
                (:) v7 v8 ->
                    let v11 = prelude_'amp'amp27 v7 v8
                    in case v11 of
                           True -> prelude_LAMBDA24 v267 v2
                           False -> prelude__foldr25 v2

prelude_LAMBDA24 v1 v2 = (:) v1 (prelude__foldr25 v2)

prelude_IO_Monad_fail41 v1 =
    overlay_errorIO
      (skipCAF realWorld# (str_ "pattern-match failure in do expression"))
      v1

prelude__foldr25 v1 =
    case v1 of
        [] -> []
        (:) v296 v297 -> prelude_LAMBDA22 v296 v297

f17 uncaf = skipCAF uncaf (str_ "Prelude.read: no parse")

f18 v1 v2 =
    case v1 of
        (,) v176 v177 ->
            case v177 of
                [] -> f20 v176 v2
                (:) v7 v8 ->
                    let v11 = prelude_'amp'amp27 v7 v8
                    in case v11 of
                           True -> f20 v176 v2
                           False ->
                               case v2 of
                                   [] -> prelude_error (f17 realWorld#)
                                   (:) v4 v5 -> f18 v4 v5

f20 v1 v2 =
    case v2 of
        [] -> v1
        (:) v257 v258 ->
            let v9 = prelude_LAMBDA22 v257 v258
            in case v9 of
                   [] -> v1
                   (:) v10 v11 ->
                       prelude_error
                         (skipCAF realWorld# (str_ "Prelude.read: ambiguous parse"))

-- Infinite loop here.  It was originally:
-- f34 v1 v2 v3 =
--    let v336 = f34 v1 v2 v3
--    in v336
--
-- But that now (correctly) just makes a non-allocating infinite loop
-- instead of (incorrectly) eta-reducing to f34 = f34.
-- So I've changed to an infinite, allocating loop, which makes
-- the heap get exhausted.
f34 v1 v2 v3 =
  if abs v2 < 1000 then 
    let v336 = f34 (v1+1) (-v2) v3
    in v336
  else if v2 == 2000 then 0 else v1

f38 v1 v2 =
    case v1 of
        [] -> system_IO_hPutChar system_IO_stdout (chr_ '\n') v2
        (:) v350 v351 ->
            case (system_IO_hPutChar
                    system_IO_stdout
                    v350
                    (typeRealWorld v2)) of
                ( v7  , v8  ) -> f38 v351 v7

main_generated v1 =
    case (system_Environment_getArgs (typeRealWorld v1)) of
        ( v3  , v4  ) ->
            case v4 of
                (:) v7 v8 ->
                    case v8 of
                        (:) v9 v12 ->
                            case v12 of
                                (:) v13 v14 ->
                                    case v14 of
                                        [] ->
                                            case (prelude_Int_Show_showsPrec
                                                    (int_ 0)
                                                    (let v8 =
                                                             case (prelude_Int_Read_readsPrec
                                                                     (int_ 0)
                                                                     v7) of
                                                                 [] -> prelude_error (f17 realWorld#)
                                                                 (:) v12 v14 -> f18 v12 v14
                                                         v10 =
                                                             case (prelude_Int_Read_readsPrec
                                                                     (int_ 0)
                                                                     v9) of
                                                                 [] -> prelude_error (f17 realWorld#)
                                                                 (:) v15 v16 -> f18 v15 v16
                                                         v11 =
                                                             case (prelude_Int_Read_readsPrec
                                                                     (int_ 0)
                                                                     v13) of
                                                                 [] -> prelude_error (f17 realWorld#)
                                                                 (:) v17 v18 -> f18 v17 v18
                                                     in case (lT_W v10 v8) of
                                                            True ->
                                                                let v7 = f34 v8 v10 v11
                                                                in v7
                                                            False -> v11)
                                                    (skipCAF realWorld# (str_ ""))) of
                                                [] ->
                                                    system_IO_hPutChar
                                                      system_IO_stdout
                                                      (chr_ '\n')
                                                      (typeRealWorld v3)
                                                (:) v11 v12 ->
                                                    case (system_IO_hPutChar
                                                            system_IO_stdout
                                                            v11
                                                            (typeRealWorld (typeRealWorld v3))) of
                                                        ( v7  , v8  ) -> f38 v12 v7
                                        (:) v15 v16 -> prelude_IO_Monad_fail41 v3
                                [] -> prelude_IO_Monad_fail41 v3
                        [] -> prelude_IO_Monad_fail41 v3
                [] -> prelude_IO_Monad_fail41 v3

