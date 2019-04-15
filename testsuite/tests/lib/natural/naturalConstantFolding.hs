{-# LANGUAGE MagicHash #-}

module Main (main) where

import Data.Bits
import GHC.Int
import GHC.Natural (Natural, intToNatural, naturalToInt, naturalToWord,
                    wordToNatural)
import GHC.Word

main :: IO ()
main = do
          p "andNatural"         andNatural
          p "bitNatural"         bitNatural
          p "minusNatural"       minusNatural
          p "naturalFromInteger" naturalFromInteger
          p "naturalToInteger"   naturalToInteger
          p "negateNatural"      negateNatural
          p "orNatural"          orNatural
          p "plusNatural"        plusNatural
          p "popCountNatural"    popCountNatural
          p "divModNatural"      divModNatural
          p "divNatural"         divNatural
          p "modNatural"         modNatural
          p "quotNatural"        quotNatural
          p "quotRemNatural"     quotRemNatural
          p "remNatural"         remNatural
          p "gcdNatural"         gcdNatural
          p "lcmNatural"         lcmNatural
          p "shiftLNatural"      shiftLNatural
          p "shiftRNatural"      shiftRNatural
          p "signumNaturalP"     signumNaturalP
          p "signumNaturalZ"     signumNaturalZ
          p "testBitNaturalT"    testBitNaturalT
          p "testBitNaturalF"    testBitNaturalF
          p "timesNatural"       timesNatural
          p "wordToNatural"      wordToNaturalLit
          p "naturalToWord"      naturalToWordLit
          p "intToNatural"       intToNaturalLit
          p "naturalToInt"       naturalToIntLit
          p "xorNatural"         xorNatural
          p "eqNatural"          eqNaturalLit
          p "neqNatural"         neqNaturalLit
          p "leNatural"          leNaturalLit
          p "ltNatural"          ltNaturalLit
          p "geNatural"          geNaturalLit
          p "gtNatural"          gtNaturalLit
          p "compareNatural"     compareNaturalLit
    where p :: Show a => String -> a -> IO ()
          p str x = putStrLn (str ++ ": " ++ show x)

-- Bit arithmetic
andNatural :: Natural
andNatural = 100052 .&. 140053

xorNatural :: Natural
xorNatural = 100071 `xor` 140072

bitNatural :: Natural
bitNatural = bit 4

orNatural :: Natural
orNatural = 100058 .|. 140059

shiftLNatural :: Natural
shiftLNatural = 100065 `shiftL` 4

shiftRNatural :: Natural
shiftRNatural = 100066 `shiftR` 4

popCountNatural :: Int
popCountNatural = popCount (100095 :: Natural)

testBitNaturalT :: Bool
testBitNaturalT = testBit (100068 :: Natural) 2

testBitNaturalF :: Bool
testBitNaturalF = testBit (100069 :: Natural) 1
-----------------------------------------------

-- Arithmetic
plusNatural :: Natural
plusNatural = 100060 + 100061

timesNatural :: Natural
timesNatural = 100070 * 6832

minusNatural :: Natural
minusNatural = 100999 - 100010

negateNatural :: Natural
negateNatural = negate 0

signumNaturalP :: Natural
signumNaturalP = signum 100067

signumNaturalZ :: Natural
signumNaturalZ = signum 0
------------------------

-- Quotients and remainders
quotRemNatural :: (Natural, Natural)
quotRemNatural = 100063 `quotRem` 123

divModNatural :: (Natural, Natural)
divModNatural = 100060 `divMod` 456

quotNatural :: Natural
quotNatural = 100062 `quot` 156

remNatural :: Natural
remNatural = 100064 `rem` 156

divNatural :: Natural
divNatural = 100286 `div` 156

modNatural :: Natural
modNatural = 100086 `mod` 156

gcdNatural :: Natural
gcdNatural = 100048 `gcd` 150072

lcmNatural :: Natural
lcmNatural = 100050 `lcm` 100060
--------------------------------

-- Conversions
naturalFromInteger :: Natural
naturalFromInteger = fromInteger 100054 + 100055

naturalToInteger :: Integer
naturalToInteger = toInteger (100056 :: Natural) + 100057

-- Same story as the @Integer@ case: for the conversion functions, we can't
-- just check that e.g. 100065 is in the resulting core, because it will be
-- regardless of whether the rules fire or not. So we add something to the
-- number being converted, and thus rely on the addition rule for the
-- end-result type also firing.
wordToNaturalLit :: Natural
wordToNaturalLit = wordToNatural 100072## + 100073

naturalToWordLit :: Word
naturalToWordLit = 100075 + W# (naturalToWord 100074)

intToNaturalLit :: Natural
intToNaturalLit = intToNatural 100076# + 100077

naturalToIntLit :: Int
naturalToIntLit = I# (naturalToInt 100078) + 100079
---------------------------------------------------

-- Ordering and Equality
eqNaturalLit, neqNaturalLit, leNaturalLit, ltNaturalLit, geNaturalLit,
    gtNaturalLit :: Bool
eqNaturalLit = (100080 :: Natural) == 100081

neqNaturalLit = (100082 :: Natural) /= 100083

leNaturalLit = (100084 :: Natural) <= 100085

ltNaturalLit = (100086 :: Natural) < 100087

geNaturalLit = (100088 :: Natural) >= 100089

gtNaturalLit = (100090 :: Natural) > 100091

compareNaturalLit :: Ordering
compareNaturalLit = compare (100092 :: Natural) 100093