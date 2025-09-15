{-# LANGUAGE CPP #-}

#include "MachDeps.h"

f :: Int -> String
f n = case n of
#if WORD_SIZE_IN_BITS == 64
  0x8000000000000000 -> "yes"
#else
  0x80000000 -> "yes"
#endif
  _ -> "no"
{-# NOINLINE f #-}

main = do
#if WORD_SIZE_IN_BITS == 64
    let string = "0x8000000000000000"
#else
    let string = "0x80000000"
#endif
    let i = read string :: Integer
    let i' = fromIntegral i :: Int
    print i
    print i'
    print (f i')
