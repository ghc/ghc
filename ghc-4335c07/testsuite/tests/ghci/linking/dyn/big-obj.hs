foreign import ccall "foo" c_foo :: IO Int

main = c_foo >>= print
