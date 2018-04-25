module WriteRoutines (outputCodes)
where

--import GlaExts
import GHC.Base	( Int(..), word2Int#, int2Word#, and#, or#, shiftL#, shiftRL# )
import Encode (CodeEvent(..))

-- Start of code added for ghc
w2i x = word2Int# x
i2w x = int2Word# x

intAnd (I# x) (I# y) = I# (w2i (and# (i2w x) (i2w y)))
intOr  (I# x) (I# y) = I# (w2i (or# (i2w x) (i2w y)))
intLsh (I# x) (I# y) = I# (w2i (shiftL# (i2w x) y))
intRsh (I# x) (I# y) = I# (w2i (shiftRL# (i2w x) y))
-- End of code added for ghc

outputCodes :: [CodeEvent] -> (String, [Int])
outputCodes cs = (map (\x -> toEnum (intAnd 255 x)) (fst result), snd result)
               where result = output 9 8 0 0 cs       -- assume 9 bit start

output :: Int -> Int -> Int -> Int -> [CodeEvent] -> ([Int], [Int])
output _ _ _ prev [] = ([prev], [1])

output nbits stillToGo r_off prev (NewWordSize : cs)
    = (fst rest, 0 : snd rest)
      where
      rest = output (nbits + 1) 8 0 0 cs
      outBits = if stillToGo /= 8 then nbits else 0

output nbits stillToGo r_off prev (Clear : cs)
    = ((prev : 1 : take' padBits padding) ++ fst rest, outBits : snd rest)
      where
      rest = output 9 8 0 0 cs
      outBits = if stillToGo /= 8 then nbits else 0
      padBits = nbits - ((9 - stillToGo) * 2)
      take' n l = if n < 0 then take 1 l else take n l

output nbits stillToGo r_off prev css@(Code code : cs)

    | stillToGo == 0 = output nbits 8 0 0 css
    | otherwise = if (nbits + r_off) >= 16 then
                      (byte1 : byte2 : fst rest1, outBits : snd rest1)
                  else
                      (byte1 : fst rest2, outBits : snd rest2)
      where
      r_off' = 8 - r_off
      byte1 = intOr prev (intLsh code r_off)
      byte2 = intRsh code r_off'
      byte3 = intRsh byte2 8
      outBits = if stillToGo == 1 then nbits else 0
      rest1 = output nbits (stillToGo-1) ((r_off+nbits) `mod` 8) byte3 cs
      rest2 = output nbits (stillToGo-1) ((r_off+nbits) `mod` 8) byte2 cs

padding :: [Int]
padding = [255, 255 ..]
