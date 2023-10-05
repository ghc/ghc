import Data.Word

foo :: Word8 -> Char
foo c | c <= 0xdf = 'A' -- 0xdf=223
      | otherwise = 'B'
{-# NOINLINE foo #-}

main = print (foo 0xce) -- 0xce=206
