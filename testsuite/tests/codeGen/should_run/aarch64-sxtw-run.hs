{-# LANGUAGE MagicHash #-}
import GHC.Exts
import GHC.Int

signExtW32 :: Int32 -> Int64
signExtW32 (I32# x) = I64# (intToInt64# (int32ToInt# x))

main :: IO ()
main = do
    print (signExtW32 (-1))
    print (signExtW32 (-128))
    print (signExtW32 0)
    print (signExtW32 127)
    print (signExtW32 (minBound :: Int32))
