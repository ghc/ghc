{-# LANGUAGE CPP #-}

#if SHOULD_PASS
message :: String
message = "Hello!"
#endif

main :: IO ()
main = putStrLn message
