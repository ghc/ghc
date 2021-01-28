module Main (main) where

import Data.Bits
import Numeric.Natural (Natural)

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
          p "wordToNatural"      wordToNatural
          p "naturalToWord"      naturalToWord
          p "intToNatural"       intToNatural
          p "naturalToInt"       naturalToInt
          p "doubleFromNatural"  doubleFromNatural
          p "floatFromNatural"   floatFromNatural
          p "xorNatural"         xorNatural
          p "eqNatural"          eqNatural
          p "neqNatural"         neqNatural
          p "leNatural"          leNatural
          p "ltNatural"          ltNatural
          p "geNatural"          geNatural
          p "gtNatural"          gtNatural
          p "compareNatural"     compareNatural

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
popCountNatural = popCount (100098 :: Natural)

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
wordToNatural :: Natural
wordToNatural = fromIntegral (100072 :: Word) + 100073

naturalToWord :: Word
naturalToWord = 100075 + fromIntegral (100074 :: Natural)

intToNatural :: Natural
intToNatural = fromIntegral (100076 :: Int) + 100077

naturalToInt :: Int
naturalToInt = fromIntegral (100078 :: Natural) + 100079

doubleFromNatural :: Double
doubleFromNatural = 100095.0 + realToFrac (100094 :: Natural)

floatFromNatural :: Float
floatFromNatural = 100097.0 + realToFrac (100096 :: Natural)

---------------------------------------------------

-- Ordering and Equality
eqNatural, neqNatural, leNatural, ltNatural, geNatural, gtNatural :: Bool
eqNatural = (100080 :: Natural) == 100081

neqNatural = (100082 :: Natural) /= 100083

leNatural = (100084 :: Natural) <= 100085

ltNatural = (100086 :: Natural) < 100087

geNatural = (100088 :: Natural) >= 100089

gtNatural = (100090 :: Natural) > 100091

compareNatural :: Ordering
compareNatural = compare (100092 :: Natural) 100093
