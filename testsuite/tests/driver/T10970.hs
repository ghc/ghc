{-# LANGUAGE CPP #-}
main = do
    putStrLn VERSION_containers
#if MIN_VERSION_base(3,0,0)
    putStrLn "OK"
#endif
