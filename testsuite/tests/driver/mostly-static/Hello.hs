foreign import ccall "my_func" c_my_func :: IO Int

main = do
    x <- c_my_func
    print x
