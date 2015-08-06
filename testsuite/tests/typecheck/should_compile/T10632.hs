{-# LANGUAGE ImplicitParams #-}

f :: (?file1 :: String) => IO ()
f = putStrLn $ "f2: "

main :: IO ()
main = let ?file1 = "A" in f
