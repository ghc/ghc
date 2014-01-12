
module Main (main) where

import Data.Bits
import Data.Int
import Data.Word

main :: IO ()
main = do p "plusInteger"        plusInteger
          p "timesInteger"       timesInteger
          p "minusIntegerN"      minusIntegerN
          p "minusIntegerP"      minusIntegerP
          p "negateInteger"      negateInteger
          p "eqIntegerE"         eqIntegerE
          p "eqIntegerN"         eqIntegerN
          p "neqIntegerE"        neqIntegerE
          p "neqIntegerN"        neqIntegerN
          p "absIntegerP"        absIntegerP
          p "absIntegerZ"        absIntegerZ
          p "absIntegerN"        absIntegerN
          p "signumIntegerP"    signumIntegerP
          p "signumIntegerZ"    signumIntegerZ
          p "signumIntegerN"    signumIntegerN
          p "leIntegerL"        leIntegerL
          p "leIntegerE"        leIntegerE
          p "leIntegerG"        leIntegerG
          p "gtIntegerL"        gtIntegerL
          p "gtIntegerE"        gtIntegerE
          p "gtIntegerG"        gtIntegerG
          p "ltIntegerL"        ltIntegerL
          p "ltIntegerE"        ltIntegerE
          p "ltIntegerG"        ltIntegerG
          p "geIntegerL"        geIntegerL
          p "geIntegerE"        geIntegerE
          p "geIntegerG"        geIntegerG
          p "compareIntegerL"   compareIntegerL
          p "compareIntegerE"   compareIntegerE
          p "compareIntegerG"   compareIntegerG
          p "gcdInteger"        gcdInteger
          p "lcmInteger"        lcmInteger
          p "andInteger"        andInteger
          p "orInteger"         orInteger
          p "xorInteger"        xorInteger
          p "complementInteger" complementInteger
          p "quotRemInteger"    quotRemInteger
          p "divModInteger"     divModInteger
          p "shiftLInteger"     shiftLInteger
          p "shiftRInteger"     shiftRInteger
          p "quotInteger"       quotInteger
          p "remInteger"        remInteger
          p "divInteger"        divInteger
          p "modInteger"        modInteger
          p "doubleFromInteger" doubleFromInteger
          p "floatFromInteger"  floatFromInteger
          p "decodeIntegerDouble" decodeIntegerDouble
          p "encodeIntegerDouble" encodeIntegerDouble
          p "encodeIntegerFloat"  encodeIntegerFloat
          p "integerToWord"       integerToWord
          p "integerToInt"        integerToInt
          p "wordToInteger"       wordToInteger
          p "intToInteger"        intToInteger
          p "word64ToInteger"     word64ToInteger
          p "int64ToInteger"      int64ToInteger

    where p :: Show a => String -> a -> IO ()
          p str x = putStrLn (str ++ ": " ++ show x)

plusInteger :: Integer
plusInteger = 100003 + 100004

timesInteger :: Integer
timesInteger = 100005 * 6832

minusIntegerN :: Integer
minusIntegerN = 100007 - 100998
minusIntegerP :: Integer
minusIntegerP = 100999 - 100010

negateInteger :: Integer
negateInteger = negate 200011

eqIntegerE :: Bool
eqIntegerE = (100012 :: Integer) == 100012
eqIntegerN :: Bool
eqIntegerN = (100013 :: Integer) == 100014

neqIntegerE :: Bool
neqIntegerE = (100015 :: Integer) /= 100015
neqIntegerN :: Bool
neqIntegerN = (100016 :: Integer) /= 100017

absIntegerP :: Integer
absIntegerP = abs 200018
absIntegerZ :: Integer
absIntegerZ = abs 0
absIntegerN :: Integer
absIntegerN = abs (-200019)

signumIntegerP :: Integer
signumIntegerP = signum 100020
signumIntegerZ :: Integer
signumIntegerZ = signum 0
signumIntegerN :: Integer
signumIntegerN = signum (-100021)

leIntegerL :: Bool
leIntegerL = (100022 :: Integer) <= 100023
leIntegerE :: Bool
leIntegerE = (100024 :: Integer) <= 100024
leIntegerG :: Bool
leIntegerG = (100026 :: Integer) <= 100025

gtIntegerL :: Bool
gtIntegerL = (100026 :: Integer) > 100027
gtIntegerE :: Bool
gtIntegerE = (100028 :: Integer) > 100028
gtIntegerG :: Bool
gtIntegerG = (100030 :: Integer) > 100031

ltIntegerL :: Bool
ltIntegerL = (100032 :: Integer) < 100033
ltIntegerE :: Bool
ltIntegerE = (100034 :: Integer) < 100034
ltIntegerG :: Bool
ltIntegerG = (100036 :: Integer) < 100035

geIntegerL :: Bool
geIntegerL = (100037 :: Integer) >= 100038
geIntegerE :: Bool
geIntegerE = (100039 :: Integer) >= 100039
geIntegerG :: Bool
geIntegerG = (100041 :: Integer) >= 100040

compareIntegerL :: Ordering
compareIntegerL = (100042 :: Integer) `compare` 100043
compareIntegerE :: Ordering
compareIntegerE = (100044 :: Integer) `compare` 100044
compareIntegerG :: Ordering
compareIntegerG = (100046 :: Integer) `compare` 100045

gcdInteger :: Integer
gcdInteger = 100048 `gcd` 150072

lcmInteger :: Integer
lcmInteger = 100050 `lcm` 100060

andInteger :: Integer
andInteger = 100052 .&. 140053

orInteger :: Integer
orInteger = 100054 .|. 140055

xorInteger :: Integer
xorInteger = 100056 `xor` 140057

complementInteger :: Integer
complementInteger = complement 200058

quotRemInteger :: (Integer, Integer)
quotRemInteger = 100059 `quotRem` 123

divModInteger :: (Integer, Integer)
divModInteger = 100060 `divMod` 456

shiftLInteger :: Integer
shiftLInteger = 100061 `shiftL` 4

shiftRInteger :: Integer
shiftRInteger = 100062 `shiftR` 4

quotInteger :: Integer
quotInteger = 100063 `quot` 156

remInteger :: Integer
remInteger = 100064 `rem` 156

divInteger :: Integer
divInteger = 100286 `div` 156

modInteger :: Integer
modInteger = 100086 `mod` 156

-- For the conversion functions, we can't just check that e.g. 100065
-- is in the resulting core, because it will be regardless of whether
-- the rules fire or not. So we add 100066, and thus rely on the
-- Double addition rule also firing.
doubleFromInteger :: Double
doubleFromInteger = fromInteger 100065 + 100066

floatFromInteger :: Float
floatFromInteger = fromInteger 100067 + 100068

encodeIntegerDouble :: Double
encodeIntegerDouble = encodeFloat 100069 2

encodeIntegerFloat :: Float
encodeIntegerFloat = encodeFloat 100070 2

integerToWord :: Word
integerToWord = fromInteger 100071 + 100072

integerToInt :: Int
integerToInt = fromInteger 100073 + 100074

wordToInteger :: Integer
wordToInteger = toInteger (100075 :: Word) + 100076

intToInteger :: Integer
intToInteger = toInteger (100077 :: Int) + 100078

word64ToInteger :: Integer
word64ToInteger = toInteger (100079 :: Word64) + 100080

int64ToInteger :: Integer
int64ToInteger = toInteger (100081 :: Int64) + 100082

decodeIntegerDouble :: (Integer, Int)
decodeIntegerDouble = decodeFloat (100083 :: Double)

