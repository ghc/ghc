{-# LANGUAGE MagicHash #-}

module Main (main) where

import Data.Bits
import GHC.Natural (Natural)

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
--           p "wordToNatural#"     wordToNatural
          p "xorNatural"         xorNatural

    where p :: Show a => String -> a -> IO ()
          p str x = putStrLn (str ++ ": " ++ show x)

andNatural :: Natural
andNatural = 100052 .&. 140053

bitNatural :: Natural
bitNatural = bit 4

minusNatural :: Natural
minusNatural = 100999 - 100010

naturalFromInteger :: Natural
naturalFromInteger = fromInteger 100054 + 100055

naturalToInteger :: Integer
naturalToInteger = toInteger (100056 :: Natural) + 100057

negateNatural :: Natural
negateNatural = negate 0

orNatural :: Natural
orNatural = 100058 .|. 140059

plusNatural :: Natural
plusNatural = 100060 + 100061

popCountNatural :: Int
popCountNatural = popCount (100095 :: Natural)

quotRemNatural :: (Natural, Natural)
quotRemNatural = 100063 `quotRem` 123

divModNatural :: (Natural, Natural)
divModNatural = 100060 `divMod` 456

divNatural :: Natural
divNatural = 100286 `div` 156

modNatural :: Natural
modNatural = 100086 `mod` 156

quotNatural :: Natural
quotNatural = 100062 `quot` 156

remNatural :: Natural
remNatural = 100064 `rem` 156

gcdNatural :: Natural
gcdNatural = 100048 `gcd` 150072

lcmNatural :: Natural
lcmNatural = 100050 `lcm` 100060

shiftLNatural :: Natural
shiftLNatural = 100065 `shiftL` 4

shiftRNatural :: Natural
shiftRNatural = 100066 `shiftR` 4

signumNaturalP :: Natural
signumNaturalP = signum 100067

signumNaturalZ :: Natural
signumNaturalZ = signum 0

testBitNaturalT :: Bool
testBitNaturalT = testBit (100068 :: Natural) 2

testBitNaturalF :: Bool
testBitNaturalF = testBit (100069 :: Natural) 1

timesNatural :: Natural
timesNatural = 100070 * 6832

-- wordToNatural :: Natural
-- wordToNatural = wordToNatural# (100075 :: Word) + 100076

xorNatural :: Natural
xorNatural = 100071 `xor` 140072
