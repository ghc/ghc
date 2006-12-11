{-# OPTIONS -fbang-patterns #-}

-- Various bang-pattern and lazy-pattern tests

module ShouldCompile where

main1,main2,main3,main4,main5,main6,main7 :: IO ()

main1 = do
    !c <- return ()
    return ()

main2 = return () >>= \ !c -> return ()

main3 = do
    (!c) <- return ()
    return ()

main4 = return () >>= \ (!c) -> return ()

main5 = let !x = 1 in return ()

main6 = do
    ~c <- return ()
    return ()

main7 = return () >>= \ ~c -> return ()


