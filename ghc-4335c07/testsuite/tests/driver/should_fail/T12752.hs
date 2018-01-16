{-# LANGUAGE CPP #-}

-- This should fail to compile with "ghc -Wcpp-undef -Werror ...".
#if this_cpp_identifier_does_not_exist
message :: String
message = "This is wrong!"
#endif

main :: IO ()
main = putStrLn "Hello"
