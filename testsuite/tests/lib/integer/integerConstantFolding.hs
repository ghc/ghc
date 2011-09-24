
module Main (main) where

import Data.Bits

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

