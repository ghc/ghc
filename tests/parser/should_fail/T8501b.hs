module Test where

bar :: IO ()
bar = mdo
    str <- return "bar"
    putStrLn str

-- Should fail
-- bar' :: IO ()
-- bar' = mdo {str <- return "bar" ; putStrLn str}
