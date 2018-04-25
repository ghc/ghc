module Test where

baz :: IO ()
baz = mdo
    putStrLn "baz"

-- Should fail
-- baz' :: IO ()
-- baz' = mdo
--     putStrLn "baz"
--     str <- return "test"

-- Should fail (and needs better error)
-- baz' :: IO ()
-- baz' = mdo {putStrLn "baz"}
