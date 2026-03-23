{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall unsafe "read_from_b" readFromB :: IO Int

main :: IO ()
main = readFromB >>= print
