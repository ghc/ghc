import B

-- See B.hs for an explanation on how this bug is triggered.

-- This is a linker error, so we have to define a main and link
main :: IO ()
main = putStrLn $ show $ bar 100 Nothing
