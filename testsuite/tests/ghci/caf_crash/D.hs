module D where

import A

data MyFunc = MyFunc String (IO Int)

funcCaf :: [MyFunc]
funcCaf = [MyFunc "square" square]

f1 :: MyFunc -> String
f1 (MyFunc s _) = s

f2 :: MyFunc -> IO Int
f2 (MyFunc s d) = d

main :: IO ()
main = do
    mainx
    putStrLn $ show $ length funcCaf
    putStrLn $ show $ f1 $ head funcCaf
    yay <- f2 $ head funcCaf
    print yay
