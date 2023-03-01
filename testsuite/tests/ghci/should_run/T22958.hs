{-# LANGUAGE MagicHash, UnboxedTuples #-}
import GHC.Exts
import GHC.IO

unit :: ()
unit = ()

i :: State# RealWorld -> (# State# RealWorld, () #)
i s = case seq# unit s of (# s', a #) -> (# s', a #)

bad :: IO ()
bad = IO i

main :: IO ()
main = bad >>= print
